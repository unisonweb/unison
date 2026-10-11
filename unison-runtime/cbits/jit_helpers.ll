; Generated from jit_helpers.c by gen-helpers.sh. Do not edit.

%struct.EnvFacts = type { i64, i32, i32, i32, i32, i64, i64, i32, i32 }
%struct.HashFacts = type { i64, i64, i64, i32 }
%struct.MH = type { i64, i32 }
%struct.ListFacts = type { i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, ptr, ptr, ptr }
%struct.RopeKind = type { i64, i64, i64, i64, i64, i64, ptr, i64, i64, i32, i32, i32, i32 }
%struct.ArrayFacts = type { %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, ptr, i32 }
%struct.WrapInfo = type { i64, i64 }

@unison_jit_EK = external global %struct.EnvFacts, align 8
@unison_jit_HK = external global %struct.HashFacts, align 8
@__const.mh_run.s = private constant %struct.MH { i64 3735928559, i32 1 }, align 8
@.str = private constant [15 x i8] c"untagged value\00", align 1
@.str.1 = private constant [9 x i8] c"type tag\00", align 1
@unison_jit_LF = external global %struct.ListFacts, align 8
@.str.3 = private constant [12 x i8] c"segment box\00", align 1
@unison_jit_TK = external global %struct.RopeKind, align 8
@unison_jit_BK = external global %struct.RopeKind, align 8
@unison_jit_AK = external global %struct.ArrayFacts, align 8
@.str.4 = private constant [8 x i8] c"foreign\00", align 1
@.str.5 = private constant [13 x i8] c"closure kind\00", align 1
@.str.6 = private constant [13 x i8] c"list element\00", align 1

define { i64, i64 } @unison_jit_murmur(i64 noundef %u, ptr noundef %b) #0 {
entry:
  %s.i = alloca %struct.MH, align 8
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 24), align 8
  %tobool.not.i = icmp eq i32 %0, 0
  br i1 %tobool.not.i, label %mh_run.exit, label %if.end.i

if.end.i:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %s.i) #6
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %s.i, ptr noundef nonnull align 8 dereferenceable(16) @__const.mh_run.s, i64 16, i1 false)
  call fastcc void @mh_val(ptr noundef nonnull %s.i, i64 noundef %u, ptr noundef %b)
  %ok1.i = getelementptr inbounds i8, ptr %s.i, i64 8
  %1 = load i32, ptr %ok1.i, align 8
  %tobool2.not.i = icmp eq i32 %1, 0
  br i1 %tobool2.not.i, label %cleanup.i, label %if.end6.i

if.end6.i:                                        ; preds = %if.end.i
  %2 = load i64, ptr %s.i, align 8
  %shr.i.i = lshr i64 %2, 47
  %xor.i.i = xor i64 %shr.i.i, %2
  %mul.i.i = mul i64 %xor.i.i, -4132994306676758123
  %shr1.i.i = lshr i64 %mul.i.i, 47
  %xor2.i.i = xor i64 %shr1.i.i, %mul.i.i
  br label %cleanup.i

cleanup.i:                                        ; preds = %if.end6.i, %if.end.i
  %retval.sroa.0.0.i = phi i64 [ 1, %if.end6.i ], [ 0, %if.end.i ]
  %retval.sroa.4.0.i = phi i64 [ %xor2.i.i, %if.end6.i ], [ 0, %if.end.i ]
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %s.i) #6
  br label %mh_run.exit

mh_run.exit:                                      ; preds = %entry, %cleanup.i
  %retval.sroa.0.1.i = phi i64 [ %retval.sroa.0.0.i, %cleanup.i ], [ 0, %entry ]
  %retval.sroa.4.1.i = phi i64 [ %retval.sroa.4.0.i, %cleanup.i ], [ 0, %entry ]
  %.fca.0.insert.i = insertvalue { i64, i64 } poison, i64 %retval.sroa.0.1.i, 0
  %.fca.1.insert.i = insertvalue { i64, i64 } %.fca.0.insert.i, i64 %retval.sroa.4.1.i, 1
  ret { i64, i64 } %.fca.1.insert.i
}

define ptr @unison_jit_resolve_dyn(ptr noundef %henv, i64 noundef %key) #0 {
entry:
  %found = alloca ptr, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %found) #6
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 44), align 4
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %cleanup67, label %if.end

if.end:                                           ; preds = %entry
  %1 = ptrtoint ptr %henv to i64
  %and.i = and i64 %1, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then1, label %if.end3

if.then1:                                         ; preds = %if.end
  %call2 = tail call ptr @unison_jit_settle(ptr noundef %henv) #6
  br label %if.end3

if.end3:                                          ; preds = %if.then1, %if.end
  %h.0 = phi ptr [ %call2, %if.then1 ], [ %henv, %if.end ]
  %2 = ptrtoint ptr %h.0 to i64
  %and.i82 = and i64 %2, 7
  %3 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 8), align 8
  %conv = sext i32 %3 to i64
  %cmp5.not = icmp eq i64 %and.i82, %conv
  br i1 %cmp5.not, label %lor.lhs.false, label %cleanup67

lor.lhs.false:                                    ; preds = %if.end3
  %and.i83 = and i64 %2, -8
  %4 = inttoptr i64 %and.i83 to ptr
  %5 = load ptr, ptr %4, align 8
  %6 = ptrtoint ptr %5 to i64
  %7 = load i64, ptr @unison_jit_EK, align 8
  %cmp8.not = icmp eq i64 %7, %6
  br i1 %cmp8.not, label %if.end11, label %cleanup67

if.end11:                                         ; preds = %lor.lhs.false
  %payload = getelementptr inbounds i8, ptr %4, i64 8
  %8 = load ptr, ptr %payload, align 8
  %arrayidx15 = getelementptr inbounds i8, ptr %4, i64 16
  %9 = load ptr, ptr %arrayidx15, align 8
  %10 = ptrtoint ptr %8 to i64
  %and.i86 = and i64 %10, 7
  %cmp17 = icmp eq i64 %and.i86, 0
  br i1 %cmp17, label %if.then19, label %if.end21

if.then19:                                        ; preds = %if.end11
  %call20 = tail call ptr @unison_jit_settle(ptr noundef %8) #6
  br label %if.end21

if.end21:                                         ; preds = %if.then19, %if.end11
  %aenv.0 = phi ptr [ %call20, %if.then19 ], [ %8, %if.end11 ]
  %11 = ptrtoint ptr %9 to i64
  %and.i87 = and i64 %11, 7
  %cmp23 = icmp eq i64 %and.i87, 0
  br i1 %cmp23, label %if.then25, label %if.end27

if.then25:                                        ; preds = %if.end21
  %call26 = tail call ptr @unison_jit_settle(ptr noundef %9) #6
  br label %if.end27

if.end27:                                         ; preds = %if.then25, %if.end21
  %denv.0 = phi ptr [ %call26, %if.then25 ], [ %9, %if.end21 ]
  %12 = ptrtoint ptr %denv.0 to i64
  %and.i88 = and i64 %12, 7
  %13 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 20), align 4
  %conv29 = sext i32 %13 to i64
  %cmp30.not = icmp eq i64 %and.i88, %conv29
  br i1 %cmp30.not, label %if.end36, label %land.lhs.true

land.lhs.true:                                    ; preds = %if.end27
  %call32 = call fastcc i32 @im_find(ptr noundef %denv.0, i64 noundef %key, ptr noundef nonnull %found, i32 noundef 0)
  %cmp33.not = icmp eq i32 %call32, 0
  br i1 %cmp33.not, label %if.end36, label %cleanup67

if.end36:                                         ; preds = %land.lhs.true, %if.end27
  %call37 = call fastcc i32 @im_find(ptr noundef %aenv.0, i64 noundef %key, ptr noundef nonnull %found, i32 noundef 0)
  %cmp38.not = icmp eq i32 %call37, 1
  br i1 %cmp38.not, label %if.end41, label %cleanup67

if.end41:                                         ; preds = %if.end36
  %14 = load ptr, ptr %found, align 8
  %15 = ptrtoint ptr %14 to i64
  %and.i89 = and i64 %15, 7
  %cmp43 = icmp eq i64 %and.i89, 0
  br i1 %cmp43, label %if.then45, label %if.end47

if.then45:                                        ; preds = %if.end41
  %call46 = call ptr @unison_jit_settle(ptr noundef %14) #6
  store ptr %call46, ptr %found, align 8
  br label %if.end47

if.end47:                                         ; preds = %if.then45, %if.end41
  %16 = load ptr, ptr %found, align 8
  %17 = ptrtoint ptr %16 to i64
  %and.i90 = and i64 %17, 7
  %cmp49.not = icmp eq i64 %and.i90, 1
  br i1 %cmp49.not, label %lor.lhs.false51, label %cleanup67

lor.lhs.false51:                                  ; preds = %if.end47
  %and.i91 = and i64 %17, -8
  %18 = inttoptr i64 %and.i91 to ptr
  %19 = load ptr, ptr %18, align 8
  %20 = ptrtoint ptr %19 to i64
  %21 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 24), align 8
  %cmp55.not = icmp eq i64 %21, %20
  br i1 %cmp55.not, label %if.end58, label %cleanup67

if.end58:                                         ; preds = %lor.lhs.false51
  %payload60 = getelementptr inbounds i8, ptr %18, i64 8
  %22 = load ptr, ptr %payload60, align 8
  %var = getelementptr inbounds i8, ptr %22, i64 8
  %23 = load atomic i64, ptr %var acquire, align 8
  %24 = inttoptr i64 %23 to ptr
  %and.i93 = and i64 %23, 7
  %cmp63 = icmp eq i64 %and.i93, 0
  br i1 %cmp63, label %cond.true, label %cleanup67

cond.true:                                        ; preds = %if.end58
  %call65 = call ptr @unison_jit_settle(ptr noundef %24) #6
  br label %cleanup67

cleanup67:                                        ; preds = %land.lhs.true, %if.end36, %lor.lhs.false51, %if.end47, %if.end58, %cond.true, %if.end3, %lor.lhs.false, %entry
  %retval.1 = phi ptr [ null, %entry ], [ null, %lor.lhs.false ], [ null, %if.end3 ], [ null, %land.lhs.true ], [ null, %if.end36 ], [ null, %lor.lhs.false51 ], [ null, %if.end47 ], [ %call65, %cond.true ], [ %24, %if.end58 ]
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %found) #6
  ret ptr %retval.1
}

declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #1

declare ptr @unison_jit_settle(ptr noundef)

define internal fastcc i32 @im_find(ptr noundef %m, i64 noundef %key, ptr noundef %out, i32 noundef %depth) #3 {
entry:
  br label %tailrecurse

tailrecurse:                                      ; preds = %if.end27, %entry
  %m.tr = phi ptr [ %m, %entry ], [ %11, %if.end27 ]
  %depth.tr = phi i32 [ %depth, %entry ], [ %add, %if.end27 ]
  %0 = ptrtoint ptr %m.tr to i64
  %1 = trunc i64 %0 to i32
  %conv = and i32 %1, 7
  %2 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 20), align 4
  %cmp = icmp eq i32 %2, %conv
  br i1 %cmp, label %cleanup33, label %if.end

if.end:                                           ; preds = %tailrecurse
  %3 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 16), align 8
  %cmp2 = icmp eq i32 %3, %conv
  br i1 %cmp2, label %if.then4, label %if.end13

if.then4:                                         ; preds = %if.end
  %and.i47 = and i64 %0, -8
  %4 = inttoptr i64 %and.i47 to ptr
  %arrayidx = getelementptr inbounds i8, ptr %4, i64 16
  %5 = load ptr, ptr %arrayidx, align 8
  %6 = ptrtoint ptr %5 to i64
  %cmp6.not = icmp eq i64 %6, %key
  br i1 %cmp6.not, label %if.end9, label %cleanup33

if.end9:                                          ; preds = %if.then4
  %payload11 = getelementptr inbounds i8, ptr %4, i64 8
  %7 = load ptr, ptr %payload11, align 8
  store ptr %7, ptr %out, align 8
  br label %cleanup33

if.end13:                                         ; preds = %if.end
  %8 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 12), align 4
  %cmp14 = icmp ne i32 %8, %conv
  %cmp16 = icmp sgt i32 %depth.tr, 64
  %or.cond = or i1 %cmp16, %cmp14
  br i1 %or.cond, label %cleanup33, label %if.end19

if.end19:                                         ; preds = %if.end13
  %and.i49 = and i64 %0, -8
  %9 = inttoptr i64 %and.i49 to ptr
  %payload21 = getelementptr inbounds i8, ptr %9, i64 8
  %10 = load ptr, ptr %payload21, align 8
  %add = add nsw i32 %depth.tr, 1
  %call23 = tail call fastcc i32 @im_find(ptr noundef %10, i64 noundef %key, ptr noundef %out, i32 noundef %add)
  %cmp24.not = icmp eq i32 %call23, 0
  br i1 %cmp24.not, label %if.end27, label %cleanup33

if.end27:                                         ; preds = %if.end19
  %arrayidx30 = getelementptr inbounds i8, ptr %9, i64 16
  %11 = load ptr, ptr %arrayidx30, align 8
  br label %tailrecurse

cleanup33:                                        ; preds = %if.end19, %if.end13, %if.then4, %tailrecurse, %if.end9
  %retval.1 = phi i32 [ 1, %if.end9 ], [ 0, %tailrecurse ], [ 0, %if.then4 ], [ -1, %if.end13 ], [ %call23, %if.end19 ]
  ret i32 %retval.1
}

declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #1

declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #4

define internal fastcc void @mh_val(ptr noundef %s, i64 noundef %u, ptr noundef %b) #0 {
entry:
  br label %tailrecurse

tailrecurse:                                      ; preds = %if.then27.i, %if.then13.i, %entry
  %u.tr = phi i64 [ %u, %entry ], [ %19, %if.then13.i ], [ %29, %if.then27.i ]
  %b.tr = phi ptr [ %b, %entry ], [ %20, %if.then13.i ], [ %30, %if.then27.i ]
  %ok = getelementptr inbounds i8, ptr %s, i64 8
  %0 = load i32, ptr %ok, align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %if.end

if.end:                                           ; preds = %tailrecurse
  %1 = ptrtoint ptr %b.tr to i64
  %and.i = and i64 %1, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then1, label %if.end3

if.then1:                                         ; preds = %if.end
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str, ptr noundef %b.tr) #6
  store i32 0, ptr %ok, align 8
  br label %return

if.end3:                                          ; preds = %if.end
  %and.i42 = and i64 %1, -8
  %2 = inttoptr i64 %and.i42 to ptr
  %3 = load ptr, ptr %2, align 8
  %4 = ptrtoint ptr %3 to i64
  %5 = load i64, ptr @unison_jit_HK, align 8
  %cmp5 = icmp eq i64 %5, %4
  br i1 %cmp5, label %if.then6, label %if.end4.i

if.then6:                                         ; preds = %if.end3
  %payload = getelementptr inbounds i8, ptr %2, i64 8
  %6 = load ptr, ptr %payload, align 8
  %7 = ptrtoint ptr %6 to i64
  %8 = trunc i64 %7 to i32
  %conv = and i32 %8, 7
  %9 = load i64, ptr %s, align 8
  %mul2.i.i = mul i64 %9, -4132994306676758123
  %xor3.i.i = xor i64 %mul2.i.i, 2224014147482003909
  store i64 %xor3.i.i, ptr %s, align 8
  switch i32 %conv, label %sw.default [
    i32 1, label %sw.bb
    i32 2, label %sw.bb9
    i32 3, label %sw.bb10
    i32 4, label %sw.bb15
  ]

sw.bb:                                            ; preds = %if.then6
  %mul2.i.i45 = mul i64 %xor3.i.i, -4132994306676758123
  %xor3.i.i46 = xor i64 %mul2.i.i45, 9176767557943398799
  %mul.i.i = mul i64 %u.tr, -4132994306676758123
  %shr.i.i = lshr i64 %mul.i.i, 47
  %xor.i.i = xor i64 %shr.i.i, %mul.i.i
  %mul1.i.i = mul i64 %xor.i.i, -4132994306676758123
  %mul2.i.i47 = mul i64 %xor3.i.i46, -4132994306676758123
  %xor3.i.i48 = xor i64 %mul2.i.i47, %mul1.i.i
  store i64 %xor3.i.i48, ptr %s, align 8
  br label %return

sw.bb9:                                           ; preds = %if.then6
  %mul2.i.i49 = mul i64 %xor3.i.i, -4132994306676758123
  %xor3.i.i50 = xor i64 %mul2.i.i49, 2101789760624275272
  %mul.i.i51 = mul i64 %u.tr, -4132994306676758123
  %shr.i.i52 = lshr i64 %mul.i.i51, 47
  %xor.i.i53 = xor i64 %shr.i.i52, %mul.i.i51
  %mul1.i.i54 = mul i64 %xor.i.i53, -4132994306676758123
  %mul2.i.i55 = mul i64 %xor3.i.i50, -4132994306676758123
  %xor3.i.i56 = xor i64 %mul2.i.i55, %mul1.i.i54
  store i64 %xor3.i.i56, ptr %s, align 8
  br label %return

sw.bb10:                                          ; preds = %if.then6
  %cmp11 = icmp sgt i64 %u.tr, -1
  %mul2.i.i57 = mul i64 %xor3.i.i, -4132994306676758123
  br i1 %cmp11, label %if.then13, label %if.else

if.then13:                                        ; preds = %sw.bb10
  %xor3.i.i58 = xor i64 %mul2.i.i57, -2104315256400255301
  %mul.i.i59 = mul i64 %u.tr, -4132994306676758123
  %shr.i.i60 = lshr i64 %mul.i.i59, 47
  %xor.i.i61 = xor i64 %shr.i.i60, %mul.i.i59
  %mul1.i.i62 = mul i64 %xor.i.i61, -4132994306676758123
  %mul2.i.i63 = mul i64 %xor3.i.i58, -4132994306676758123
  %xor3.i.i64 = xor i64 %mul2.i.i63, %mul1.i.i62
  store i64 %xor3.i.i64, ptr %s, align 8
  br label %return

if.else:                                          ; preds = %sw.bb10
  %xor3.i.i66 = xor i64 %mul2.i.i57, -4752061681754765867
  %mul.i.i67 = mul i64 %u.tr, 4132994306676758123
  %shr.i.i68 = lshr i64 %mul.i.i67, 47
  %xor.i.i69 = xor i64 %shr.i.i68, %mul.i.i67
  %mul1.i.i70 = mul i64 %xor.i.i69, -4132994306676758123
  %mul2.i.i71 = mul i64 %xor3.i.i66, -4132994306676758123
  %xor3.i.i72 = xor i64 %mul2.i.i71, %mul1.i.i70
  store i64 %xor3.i.i72, ptr %s, align 8
  br label %return

sw.bb15:                                          ; preds = %if.then6
  %mul2.i.i73 = mul i64 %xor3.i.i, -4132994306676758123
  %xor3.i.i74 = xor i64 %mul2.i.i73, -2104315256400255301
  %mul.i.i75 = mul i64 %u.tr, -4132994306676758123
  %shr.i.i76 = lshr i64 %mul.i.i75, 47
  %xor.i.i77 = xor i64 %shr.i.i76, %mul.i.i75
  %mul1.i.i78 = mul i64 %xor.i.i77, -4132994306676758123
  %mul2.i.i79 = mul i64 %xor3.i.i74, -4132994306676758123
  %xor3.i.i80 = xor i64 %mul2.i.i79, %mul1.i.i78
  store i64 %xor3.i.i80, ptr %s, align 8
  br label %return

sw.default:                                       ; preds = %if.then6
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.1, ptr noundef %b.tr) #6
  store i32 0, ptr %ok, align 8
  br label %return

if.end4.i:                                        ; preds = %if.end3
  %10 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp7.i = icmp eq i64 %10, %4
  br i1 %cmp7.i, label %if.then9.i, label %if.else.i

if.then9.i:                                       ; preds = %if.end4.i
  %11 = load i64, ptr %s, align 8
  %mul2.i.i155 = mul i64 %11, -4132994306676758123
  %xor3.i.i156 = xor i64 %mul2.i.i155, -8896591755956199400
  store i64 %xor3.i.i156, ptr %s, align 8
  %arrayidx.i = getelementptr inbounds i8, ptr %2, i64 16
  %12 = load ptr, ptr %arrayidx.i, align 8
  %13 = ptrtoint ptr %12 to i64
  %and.i81 = and i64 %13, 65535
  %mul.i.i148 = mul i64 %and.i81, -4132994306676758123
  %shr.i.i149 = lshr i64 %mul.i.i148, 47
  %xor.i.i150 = xor i64 %shr.i.i149, %mul.i.i148
  %mul1.i.i151 = mul i64 %xor.i.i150, -4132994306676758123
  %mul2.i.i152 = mul i64 %xor3.i.i156, -4132994306676758123
  %xor3.i.i153 = xor i64 %mul1.i.i151, %mul2.i.i152
  store i64 %xor3.i.i153, ptr %s, align 8
  br label %return

if.else.i:                                        ; preds = %if.end4.i
  %14 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp11.i = icmp eq i64 %14, %4
  br i1 %cmp11.i, label %if.then13.i, label %if.else24.i

if.then13.i:                                      ; preds = %if.else.i
  %15 = load i64, ptr %s, align 8
  %mul2.i.i146 = mul i64 %15, -4132994306676758123
  %xor3.i.i147 = xor i64 %mul2.i.i146, -8896591755956199400
  store i64 %xor3.i.i147, ptr %s, align 8
  %arrayidx16.i = getelementptr inbounds i8, ptr %2, i64 24
  %16 = load ptr, ptr %arrayidx16.i, align 8
  %17 = ptrtoint ptr %16 to i64
  %and17.i = and i64 %17, 65535
  %mul.i.i139 = mul i64 %and17.i, -4132994306676758123
  %shr.i.i140 = lshr i64 %mul.i.i139, 47
  %xor.i.i141 = xor i64 %shr.i.i140, %mul.i.i139
  %mul1.i.i142 = mul i64 %xor.i.i141, -4132994306676758123
  %mul2.i.i143 = mul i64 %xor3.i.i147, -4132994306676758123
  %xor3.i.i144 = xor i64 %mul1.i.i142, %mul2.i.i143
  store i64 %xor3.i.i144, ptr %s, align 8
  %arrayidx20.i = getelementptr inbounds i8, ptr %2, i64 32
  %18 = load ptr, ptr %arrayidx20.i, align 8
  %19 = ptrtoint ptr %18 to i64
  %arrayidx23.i = getelementptr inbounds i8, ptr %2, i64 16
  %20 = load ptr, ptr %arrayidx23.i, align 8
  br label %tailrecurse

if.else24.i:                                      ; preds = %if.else.i
  %21 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp25.i = icmp eq i64 %21, %4
  br i1 %cmp25.i, label %if.then27.i, label %if.else44.i

if.then27.i:                                      ; preds = %if.else24.i
  %22 = load i64, ptr %s, align 8
  %mul2.i.i135 = mul i64 %22, -4132994306676758123
  %xor3.i.i136 = xor i64 %mul2.i.i135, -8896591755956199400
  store i64 %xor3.i.i136, ptr %s, align 8
  %arrayidx30.i = getelementptr inbounds i8, ptr %2, i64 32
  %23 = load ptr, ptr %arrayidx30.i, align 8
  %24 = ptrtoint ptr %23 to i64
  %and31.i = and i64 %24, 65535
  %mul.i.i128 = mul i64 %and31.i, -4132994306676758123
  %shr.i.i129 = lshr i64 %mul.i.i128, 47
  %xor.i.i130 = xor i64 %shr.i.i129, %mul.i.i128
  %mul1.i.i131 = mul i64 %xor.i.i130, -4132994306676758123
  %mul2.i.i132 = mul i64 %xor3.i.i136, -4132994306676758123
  %xor3.i.i133 = xor i64 %mul1.i.i131, %mul2.i.i132
  store i64 %xor3.i.i133, ptr %s, align 8
  %arrayidx34.i = getelementptr inbounds i8, ptr %2, i64 40
  %25 = load ptr, ptr %arrayidx34.i, align 8
  %26 = ptrtoint ptr %25 to i64
  %arrayidx37.i = getelementptr inbounds i8, ptr %2, i64 16
  %27 = load ptr, ptr %arrayidx37.i, align 8
  tail call fastcc void @mh_val(ptr noundef nonnull %s, i64 noundef %26, ptr noundef %27)
  %arrayidx40.i = getelementptr inbounds i8, ptr %2, i64 48
  %28 = load ptr, ptr %arrayidx40.i, align 8
  %29 = ptrtoint ptr %28 to i64
  %arrayidx43.i = getelementptr inbounds i8, ptr %2, i64 24
  %30 = load ptr, ptr %arrayidx43.i, align 8
  br label %tailrecurse

if.else44.i:                                      ; preds = %if.else24.i
  %31 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 16), align 8
  %cmp45.i = icmp eq i64 %31, %4
  br i1 %cmp45.i, label %if.then47.i, label %if.else88.i

if.then47.i:                                      ; preds = %if.else44.i
  %arrayidx50.i = getelementptr inbounds i8, ptr %2, i64 16
  %32 = load ptr, ptr %arrayidx50.i, align 8
  %call51.i = tail call ptr @unison_jit_settle(ptr noundef %32) #6
  %arrayidx54.i = getelementptr inbounds i8, ptr %2, i64 24
  %33 = load ptr, ptr %arrayidx54.i, align 8
  %call55.i = tail call ptr @unison_jit_settle(ptr noundef %33) #6
  %34 = ptrtoint ptr %call51.i to i64
  %and.i121 = and i64 %34, 7
  %cmp57.i = icmp eq i64 %and.i121, 0
  br i1 %cmp57.i, label %if.then62.i, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %if.then47.i
  %35 = ptrtoint ptr %call55.i to i64
  %and.i120 = and i64 %35, 7
  %cmp60.i = icmp eq i64 %and.i120, 0
  br i1 %cmp60.i, label %if.then62.i, label %if.end67.i

if.then62.i:                                      ; preds = %lor.lhs.false.i, %if.then47.i
  %cond.i = phi ptr [ %call55.i, %lor.lhs.false.i ], [ %call51.i, %if.then47.i ]
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.3, ptr noundef %cond.i) #6
  store i32 0, ptr %ok, align 8
  br label %return

if.end67.i:                                       ; preds = %lor.lhs.false.i
  %36 = load i64, ptr %s, align 8
  %mul2.i.i117 = mul i64 %36, -4132994306676758123
  %xor3.i.i118 = xor i64 %mul2.i.i117, -8896591755956199400
  store i64 %xor3.i.i118, ptr %s, align 8
  %arrayidx70.i = getelementptr inbounds i8, ptr %2, i64 32
  %37 = load ptr, ptr %arrayidx70.i, align 8
  %38 = ptrtoint ptr %37 to i64
  %and71.i = and i64 %38, 65535
  %mul.i.i110 = mul i64 %and71.i, -4132994306676758123
  %shr.i.i111 = lshr i64 %mul.i.i110, 47
  %xor.i.i112 = xor i64 %shr.i.i111, %mul.i.i110
  %mul1.i.i113 = mul i64 %xor.i.i112, -4132994306676758123
  %mul2.i.i114 = mul i64 %xor3.i.i118, -4132994306676758123
  %xor3.i.i115 = xor i64 %mul1.i.i113, %mul2.i.i114
  store i64 %xor3.i.i115, ptr %s, align 8
  %and.i108 = and i64 %35, -8
  %39 = inttoptr i64 %and.i108 to ptr
  %payload76.i = getelementptr inbounds i8, ptr %39, i64 8
  %40 = load ptr, ptr %payload76.i, align 8
  %ptrs.i = getelementptr inbounds i8, ptr %40, i64 8
  %41 = load i64, ptr %ptrs.i, align 8
  %cmp78.i165.not = icmp eq i64 %41, 0
  br i1 %cmp78.i165.not, label %return, label %land.rhs.i.lr.ph

land.rhs.i.lr.ph:                                 ; preds = %if.end67.i
  %and.i109 = and i64 %34, -8
  %42 = inttoptr i64 %and.i109 to ptr
  %payload73.i = getelementptr inbounds i8, ptr %42, i64 8
  %43 = load ptr, ptr %payload73.i, align 8
  %payload83.i = getelementptr inbounds i8, ptr %43, i64 16
  %payload85.i = getelementptr inbounds i8, ptr %40, i64 24
  br label %land.rhs.i

land.rhs.i:                                       ; preds = %land.rhs.i.lr.ph, %for.body.i
  %j.0.i166 = phi i64 [ 0, %land.rhs.i.lr.ph ], [ %inc.i, %for.body.i ]
  %44 = load i32, ptr %ok, align 8
  %tobool81.not.i = icmp eq i32 %44, 0
  br i1 %tobool81.not.i, label %return, label %for.body.i

for.body.i:                                       ; preds = %land.rhs.i
  %45 = xor i64 %j.0.i166, -1
  %sub82.i = add i64 %41, %45
  %arrayidx84.i = getelementptr inbounds i64, ptr %payload83.i, i64 %sub82.i
  %46 = load i64, ptr %arrayidx84.i, align 8
  %arrayidx86.i = getelementptr inbounds [0 x ptr], ptr %payload85.i, i64 0, i64 %sub82.i
  %47 = load ptr, ptr %arrayidx86.i, align 8
  tail call fastcc void @mh_val(ptr noundef nonnull %s, i64 noundef %46, ptr noundef %47)
  %inc.i = add nuw i64 %j.0.i166, 1
  %exitcond.not = icmp eq i64 %inc.i, %41
  br i1 %exitcond.not, label %return, label %land.rhs.i

if.else88.i:                                      ; preds = %if.else44.i
  %48 = load i64, ptr @unison_jit_LF, align 8
  %cmp89.i = icmp eq i64 %48, %4
  br i1 %cmp89.i, label %if.then91.i, label %if.else160.i

if.then91.i:                                      ; preds = %if.else88.i
  %call92.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_TK, ptr noundef %b.tr)
  %cmp93.not.i = icmp eq ptr %call92.i, null
  br i1 %cmp93.not.i, label %if.else96.i, label %if.then95.i

if.then95.i:                                      ; preds = %if.then91.i
  %49 = load i64, ptr %s, align 8
  %mul2.i.i106 = mul i64 %49, -4132994306676758123
  %xor3.i.i107 = xor i64 %mul2.i.i106, 2224014147482003909
  %mul2.i.i104 = mul i64 %xor3.i.i107, -4132994306676758123
  %xor3.i.i105 = xor i64 %mul2.i.i104, 1923352212695871500
  store i64 %xor3.i.i105, ptr %s, align 8
  tail call fastcc void @mh_text(ptr noundef nonnull %s, ptr noundef nonnull %call92.i)
  br label %return

if.else96.i:                                      ; preds = %if.then91.i
  %call97.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_BK, ptr noundef %b.tr)
  %cmp98.not.i = icmp eq ptr %call97.i, null
  br i1 %cmp98.not.i, label %if.else101.i, label %if.then100.i

if.then100.i:                                     ; preds = %if.else96.i
  %50 = load i64, ptr %s, align 8
  %mul2.i.i102 = mul i64 %50, -4132994306676758123
  %xor3.i.i103 = xor i64 %mul2.i.i102, 2224014147482003909
  %mul2.i.i100 = mul i64 %xor3.i.i103, -4132994306676758123
  %xor3.i.i101 = xor i64 %mul2.i.i100, -1837384421042553197
  store i64 %xor3.i.i101, ptr %s, align 8
  tail call fastcc void @mh_bytes(ptr noundef nonnull %s, ptr noundef nonnull %call97.i)
  br label %return

if.else101.i:                                     ; preds = %if.else96.i
  %call102.i = tail call fastcc ptr @deque_of(ptr noundef %b.tr)
  %cmp103.not.i = icmp eq ptr %call102.i, null
  br i1 %cmp103.not.i, label %if.else106.i, label %if.then105.i

if.then105.i:                                     ; preds = %if.else101.i
  %51 = load i64, ptr %s, align 8
  %mul2.i.i98 = mul i64 %51, -4132994306676758123
  %xor3.i.i99 = xor i64 %mul2.i.i98, 2224014147482003909
  %mul2.i.i96 = mul i64 %xor3.i.i99, -4132994306676758123
  %xor3.i.i97 = xor i64 %mul2.i.i96, -8896591755956199400
  store i64 %xor3.i.i97, ptr %s, align 8
  tail call void @unison_jit_lv_each(ptr noundef nonnull %call102.i, i32 noundef 1, i32 noundef 0, ptr noundef nonnull @mh_val_closure, ptr noundef nonnull %s) #6
  br label %return

if.else106.i:                                     ; preds = %if.else101.i
  %52 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_AK, i64 120), align 8
  %tobool107.not.i = icmp eq i32 %52, 0
  br i1 %tobool107.not.i, label %if.else153.i, label %land.lhs.true.i

land.lhs.true.i:                                  ; preds = %if.else106.i
  %call108.i = tail call fastcc ptr @wrapped_of(ptr noundef %b.tr, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 48))
  %cmp109.not.i = icmp eq ptr %call108.i, null
  br i1 %cmp109.not.i, label %land.lhs.true129.i, label %if.then111.i

if.then111.i:                                     ; preds = %land.lhs.true.i
  %53 = load i64, ptr %s, align 8
  %mul2.i.i94 = mul i64 %53, -4132994306676758123
  %xor3.i.i95 = xor i64 %mul2.i.i94, 2224014147482003909
  %mul2.i.i92 = mul i64 %xor3.i.i95, -4132994306676758123
  %xor3.i.i93 = xor i64 %mul2.i.i92, 4448028294964007818
  store i64 %xor3.i.i93, ptr %s, align 8
  %bytes.i = getelementptr inbounds i8, ptr %call108.i, i64 8
  %54 = load i64, ptr %bytes.i, align 8
  %cmp115.i161.not = icmp eq i64 %54, 0
  br i1 %cmp115.i161.not, label %return, label %for.body118.i.lr.ph

for.body118.i.lr.ph:                              ; preds = %if.then111.i
  %payload119.i = getelementptr inbounds i8, ptr %call108.i, i64 16
  br label %for.body118.i

for.body118.i:                                    ; preds = %for.body118.i.lr.ph, %for.body118.i
  %i113.0.i162 = phi i64 [ 0, %for.body118.i.lr.ph ], [ %inc124.i, %for.body118.i ]
  %arrayidx121.i = getelementptr inbounds i8, ptr %payload119.i, i64 %i113.0.i162
  %55 = load i8, ptr %arrayidx121.i, align 1
  %conv122.i = zext i8 %55 to i64
  %56 = load i64, ptr %s, align 8
  %mul.i.i86 = mul i64 %conv122.i, -4132994306676758123
  %shr.i.i87 = lshr i64 %mul.i.i86, 47
  %xor.i.i88 = xor i64 %shr.i.i87, %mul.i.i86
  %mul1.i.i89 = mul i64 %xor.i.i88, -4132994306676758123
  %mul2.i.i90 = mul i64 %56, -4132994306676758123
  %xor3.i.i91 = xor i64 %mul1.i.i89, %mul2.i.i90
  store i64 %xor3.i.i91, ptr %s, align 8
  %inc124.i = add nuw i64 %i113.0.i162, 1
  %57 = load i64, ptr %bytes.i, align 8
  %cmp115.i = icmp ult i64 %inc124.i, %57
  br i1 %cmp115.i, label %for.body118.i, label %return

land.lhs.true129.i:                               ; preds = %land.lhs.true.i
  %call130.i = tail call fastcc ptr @wrapped_of(ptr noundef %b.tr, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 16))
  %cmp131.not.i = icmp eq ptr %call130.i, null
  br i1 %cmp131.not.i, label %if.else153.i, label %if.then133.i

if.then133.i:                                     ; preds = %land.lhs.true129.i
  %58 = load i64, ptr %s, align 8
  %mul2.i.i84 = mul i64 %58, -4132994306676758123
  %xor3.i.i85 = xor i64 %mul2.i.i84, 2224014147482003909
  %mul2.i.i82 = mul i64 %xor3.i.i85, -4132994306676758123
  %xor3.i.i83 = xor i64 %mul2.i.i82, 5840509881662445638
  store i64 %xor3.i.i83, ptr %s, align 8
  %ptrs138.i = getelementptr inbounds i8, ptr %call130.i, i64 8
  %59 = load i64, ptr %ptrs138.i, align 8
  %cmp139.i163.not = icmp eq i64 %59, 0
  br i1 %cmp139.i163.not, label %return, label %land.rhs141.i.lr.ph

land.rhs141.i.lr.ph:                              ; preds = %if.then133.i
  %payload147.i = getelementptr inbounds i8, ptr %call130.i, i64 24
  br label %land.rhs141.i

land.rhs141.i:                                    ; preds = %land.rhs141.i.lr.ph, %for.body146.i
  %i136.0.i164 = phi i64 [ 0, %land.rhs141.i.lr.ph ], [ %inc150.i, %for.body146.i ]
  %60 = load i32, ptr %ok, align 8
  %tobool143.not.i = icmp eq i32 %60, 0
  br i1 %tobool143.not.i, label %return, label %for.body146.i

for.body146.i:                                    ; preds = %land.rhs141.i
  %arrayidx148.i = getelementptr inbounds [0 x ptr], ptr %payload147.i, i64 0, i64 %i136.0.i164
  %61 = load ptr, ptr %arrayidx148.i, align 8
  tail call void @mh_val_closure(ptr noundef %61, ptr noundef %s)
  %inc150.i = add nuw i64 %i136.0.i164, 1
  %62 = load i64, ptr %ptrs138.i, align 8
  %cmp139.i = icmp ult i64 %inc150.i, %62
  br i1 %cmp139.i, label %land.rhs141.i, label %return

if.else153.i:                                     ; preds = %if.else106.i, %land.lhs.true129.i
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.4, ptr noundef %b.tr) #6
  store i32 0, ptr %ok, align 8
  br label %return

if.else160.i:                                     ; preds = %if.else88.i
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.5, ptr noundef %b.tr) #6
  store i32 0, ptr %ok, align 8
  br label %return

return:                                           ; preds = %for.body118.i, %for.body146.i, %land.rhs141.i, %for.body.i, %land.rhs.i, %if.then111.i, %if.then133.i, %if.end67.i, %if.else160.i, %if.else153.i, %if.then105.i, %if.then100.i, %if.then95.i, %if.then62.i, %if.then9.i, %sw.bb, %sw.bb9, %sw.bb15, %sw.default, %if.else, %if.then13, %tailrecurse, %if.then1
  ret void
}

declare void @unison_jit_murmur_fail(ptr noundef, ptr noundef)

define internal fastcc ptr @rope_of(ptr nocapture noundef readonly %K, ptr noundef %x) #5 {
entry:
  %0 = ptrtoint ptr %x to i64
  %and.i = and i64 %0, 7
  %cmp.not = icmp eq i64 %and.i, 7
  br i1 %cmp.not, label %lor.lhs.false, label %return

lor.lhs.false:                                    ; preds = %entry
  %and.i22 = and i64 %0, -8
  %1 = inttoptr i64 %and.i22 to ptr
  %2 = load ptr, ptr %1, align 8
  %3 = ptrtoint ptr %2 to i64
  %4 = load i64, ptr %K, align 8
  %cmp2.not = icmp eq i64 %4, %3
  br i1 %cmp2.not, label %if.end, label %return

if.end:                                           ; preds = %lor.lhs.false
  %payload = getelementptr inbounds i8, ptr %1, i64 8
  %5 = load ptr, ptr %payload, align 8
  %6 = ptrtoint ptr %5 to i64
  %and.i24 = and i64 %6, 7
  %wrap_tag = getelementptr inbounds i8, ptr %K, i64 16
  %7 = load i64, ptr %wrap_tag, align 8
  %cmp5.not = icmp eq i64 %and.i24, %7
  br i1 %cmp5.not, label %lor.lhs.false6, label %return

lor.lhs.false6:                                   ; preds = %if.end
  %and.i25 = and i64 %6, -8
  %8 = inttoptr i64 %and.i25 to ptr
  %9 = load ptr, ptr %8, align 8
  %10 = ptrtoint ptr %9 to i64
  %wrap_info = getelementptr inbounds i8, ptr %K, i64 8
  %11 = load i64, ptr %wrap_info, align 8
  %cmp10.not = icmp eq i64 %11, %10
  br i1 %cmp10.not, label %if.end12, label %return

if.end12:                                         ; preds = %lor.lhs.false6
  %payload14 = getelementptr inbounds i8, ptr %8, i64 8
  %12 = load ptr, ptr %payload14, align 8
  br label %return

return:                                           ; preds = %if.end12, %lor.lhs.false6, %if.end, %entry, %lor.lhs.false
  %retval.1 = phi ptr [ null, %lor.lhs.false ], [ null, %entry ], [ %12, %if.end12 ], [ null, %lor.lhs.false6 ], [ null, %if.end ]
  ret ptr %retval.1
}

define internal fastcc void @mh_text(ptr nocapture noundef %s, ptr noundef %r) #0 {
entry:
  %off = alloca i64, align 8
  %0 = ptrtoint ptr %r to i64
  %and.i.i = and i64 %0, 7
  switch i64 %and.i.i, label %rope_size.exit [
    i64 2, label %sw.bb.i
    i64 3, label %sw.bb3.i
  ]

sw.bb.i:                                          ; preds = %entry
  %and.i9.i = and i64 %0, -8
  %1 = inttoptr i64 %and.i9.i to ptr
  %payload.i = getelementptr inbounds i8, ptr %1, i64 8
  %2 = load ptr, ptr %payload.i, align 8
  %3 = ptrtoint ptr %2 to i64
  %and.i.i.i = and i64 %3, -8
  %4 = inttoptr i64 %and.i.i.i to ptr
  %payload.i.i = getelementptr inbounds i8, ptr %4, i64 8
  %5 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_TK, i64 72), align 8
  %idxprom.i.i = sext i32 %5 to i64
  %arrayidx.i.i = getelementptr inbounds [0 x ptr], ptr %payload.i.i, i64 0, i64 %idxprom.i.i
  %6 = load ptr, ptr %arrayidx.i.i, align 8
  %7 = ptrtoint ptr %6 to i64
  br label %rope_size.exit

sw.bb3.i:                                         ; preds = %entry
  %and.i10.i = and i64 %0, -8
  %8 = inttoptr i64 %and.i10.i to ptr
  %arrayidx6.i = getelementptr inbounds i8, ptr %8, i64 32
  %9 = load ptr, ptr %arrayidx6.i, align 8
  %10 = ptrtoint ptr %9 to i64
  %shr.i = ashr i64 %10, 8
  br label %rope_size.exit

rope_size.exit:                                   ; preds = %entry, %sw.bb.i, %sw.bb3.i
  %retval.0.i = phi i64 [ %shr.i, %sw.bb3.i ], [ %7, %sw.bb.i ], [ 0, %entry ]
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %off) #6
  %cmp107 = icmp sgt i64 %retval.0.i, 0
  br i1 %cmp107, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %while.end, %rope_size.exit
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %off) #6
  ret void

for.body:                                         ; preds = %rope_size.exit, %while.end
  %i.0108 = phi i64 [ %add, %while.end ], [ 0, %rope_size.exit ]
  %call1 = call ptr @unison_jit_rope_chunk_at(ptr noundef nonnull @unison_jit_TK, ptr noundef %r, i64 noundef %i.0108, ptr noundef nonnull %off) #6
  %11 = ptrtoint ptr %call1 to i64
  %and.i.i94 = and i64 %11, -8
  %12 = inttoptr i64 %and.i.i94 to ptr
  %payload.i95 = getelementptr inbounds i8, ptr %12, i64 8
  %13 = load ptr, ptr %payload.i95, align 8
  %payload1.i = getelementptr inbounds i8, ptr %13, i64 16
  %14 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_TK, i64 76), align 4
  %idxprom.i.i96 = sext i32 %14 to i64
  %arrayidx.i.i97 = getelementptr inbounds [0 x ptr], ptr %payload.i95, i64 0, i64 %idxprom.i.i96
  %15 = load ptr, ptr %arrayidx.i.i97, align 8
  %16 = ptrtoint ptr %15 to i64
  %add.ptr.i = getelementptr inbounds i8, ptr %payload1.i, i64 %16
  %17 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_TK, i64 80), align 8
  %idxprom.i = sext i32 %17 to i64
  %arrayidx.i = getelementptr inbounds [0 x ptr], ptr %payload.i95, i64 0, i64 %idxprom.i
  %18 = load ptr, ptr %arrayidx.i, align 8
  %19 = ptrtoint ptr %18 to i64
  %add.ptr = getelementptr inbounds i8, ptr %add.ptr.i, i64 %19
  %cmp4104 = icmp sgt ptr %18, null
  br i1 %cmp4104, label %while.body.lr.ph, label %while.end

while.body.lr.ph:                                 ; preds = %for.body
  %s.promoted = load i64, ptr %s, align 8
  br label %while.body

while.body:                                       ; preds = %while.body.lr.ph, %if.end65
  %xor3.i.i106 = phi i64 [ %s.promoted, %while.body.lr.ph ], [ %xor3.i.i, %if.end65 ]
  %p.0105 = phi ptr [ %add.ptr.i, %while.body.lr.ph ], [ %p.1, %if.end65 ]
  %20 = load i8, ptr %p.0105, align 1
  %conv = zext i8 %20 to i32
  %cmp5 = icmp sgt i8 %20, -1
  br i1 %cmp5, label %if.then, label %if.else

if.then:                                          ; preds = %while.body
  %conv8 = zext nneg i8 %20 to i64
  %add.ptr9 = getelementptr inbounds i8, ptr %p.0105, i64 1
  br label %if.end65

if.else:                                          ; preds = %while.body
  %cmp12 = icmp ult i8 %20, -32
  br i1 %cmp12, label %if.then14, label %if.else22

if.then14:                                        ; preds = %if.else
  %and = shl nuw nsw i32 %conv, 6
  %shl = and i32 %and, 1984
  %arrayidx17 = getelementptr inbounds i8, ptr %p.0105, i64 1
  %21 = load i8, ptr %arrayidx17, align 1
  %22 = and i8 %21, 63
  %and19 = zext nneg i8 %22 to i32
  %or = or disjoint i32 %shl, %and19
  %conv20 = zext nneg i32 %or to i64
  %add.ptr21 = getelementptr inbounds i8, ptr %p.0105, i64 2
  br label %if.end65

if.else22:                                        ; preds = %if.else
  %cmp25 = icmp ult i8 %20, -16
  %arrayidx32 = getelementptr inbounds i8, ptr %p.0105, i64 1
  %23 = load i8, ptr %arrayidx32, align 1
  %24 = and i8 %23, 63
  %and34 = zext nneg i8 %24 to i32
  %arrayidx37 = getelementptr inbounds i8, ptr %p.0105, i64 2
  %25 = load i8, ptr %arrayidx37, align 1
  %26 = and i8 %25, 63
  %and39 = zext nneg i8 %26 to i32
  %add.ptr42 = getelementptr inbounds i8, ptr %p.0105, i64 3
  br i1 %cmp25, label %if.then27, label %if.else43

if.then27:                                        ; preds = %if.else22
  %and30 = shl nuw nsw i32 %conv, 12
  %shl31 = and i32 %and30, 61440
  %shl35 = shl nuw nsw i32 %and34, 6
  %or36 = or disjoint i32 %shl35, %shl31
  %or40 = or disjoint i32 %or36, %and39
  %conv41 = zext nneg i32 %or40 to i64
  br label %if.end65

if.else43:                                        ; preds = %if.else22
  %and46 = shl nuw nsw i32 %conv, 18
  %shl47 = and i32 %and46, 1835008
  %shl51 = shl nuw nsw i32 %and34, 12
  %or52 = or disjoint i32 %shl51, %shl47
  %shl56 = shl nuw nsw i32 %and39, 6
  %or57 = or disjoint i32 %or52, %shl56
  %27 = load i8, ptr %add.ptr42, align 1
  %28 = and i8 %27, 63
  %and60 = zext nneg i8 %28 to i32
  %or61 = or disjoint i32 %or57, %and60
  %conv62 = zext nneg i32 %or61 to i64
  %add.ptr63 = getelementptr inbounds i8, ptr %p.0105, i64 4
  br label %if.end65

if.end65:                                         ; preds = %if.then14, %if.else43, %if.then27, %if.then
  %p.1 = phi ptr [ %add.ptr9, %if.then ], [ %add.ptr21, %if.then14 ], [ %add.ptr42, %if.then27 ], [ %add.ptr63, %if.else43 ]
  %cp.0 = phi i64 [ %conv8, %if.then ], [ %conv20, %if.then14 ], [ %conv41, %if.then27 ], [ %conv62, %if.else43 ]
  %mul.i.i = mul i64 %cp.0, -4132994306676758123
  %shr.i.i = lshr i64 %mul.i.i, 47
  %xor.i.i = xor i64 %shr.i.i, %mul.i.i
  %mul1.i.i = mul i64 %xor.i.i, -4132994306676758123
  %mul2.i.i = mul i64 %xor3.i.i106, -4132994306676758123
  %xor3.i.i = xor i64 %mul1.i.i, %mul2.i.i
  store i64 %xor3.i.i, ptr %s, align 8
  %cmp4 = icmp ult ptr %p.1, %add.ptr
  br i1 %cmp4, label %while.body, label %while.end

while.end:                                        ; preds = %if.end65, %for.body
  %29 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_TK, i64 72), align 8
  %idxprom.i102 = sext i32 %29 to i64
  %arrayidx.i103 = getelementptr inbounds [0 x ptr], ptr %payload.i95, i64 0, i64 %idxprom.i102
  %30 = load ptr, ptr %arrayidx.i103, align 8
  %31 = ptrtoint ptr %30 to i64
  %add = add nsw i64 %i.0108, %31
  %cmp = icmp slt i64 %add, %retval.0.i
  br i1 %cmp, label %for.body, label %for.cond.cleanup
}

define internal fastcc void @mh_bytes(ptr nocapture noundef %s, ptr noundef %r) #0 {
entry:
  %off = alloca i64, align 8
  %0 = ptrtoint ptr %r to i64
  %and.i.i = and i64 %0, 7
  switch i64 %and.i.i, label %rope_size.exit [
    i64 2, label %sw.bb.i
    i64 3, label %sw.bb3.i
  ]

sw.bb.i:                                          ; preds = %entry
  %and.i9.i = and i64 %0, -8
  %1 = inttoptr i64 %and.i9.i to ptr
  %payload.i = getelementptr inbounds i8, ptr %1, i64 8
  %2 = load ptr, ptr %payload.i, align 8
  %3 = ptrtoint ptr %2 to i64
  %and.i.i.i = and i64 %3, -8
  %4 = inttoptr i64 %and.i.i.i to ptr
  %payload.i.i = getelementptr inbounds i8, ptr %4, i64 8
  %5 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_BK, i64 72), align 8
  %idxprom.i.i = sext i32 %5 to i64
  %arrayidx.i.i = getelementptr inbounds [0 x ptr], ptr %payload.i.i, i64 0, i64 %idxprom.i.i
  %6 = load ptr, ptr %arrayidx.i.i, align 8
  %7 = ptrtoint ptr %6 to i64
  br label %rope_size.exit

sw.bb3.i:                                         ; preds = %entry
  %and.i10.i = and i64 %0, -8
  %8 = inttoptr i64 %and.i10.i to ptr
  %arrayidx6.i = getelementptr inbounds i8, ptr %8, i64 32
  %9 = load ptr, ptr %arrayidx6.i, align 8
  %10 = ptrtoint ptr %9 to i64
  %shr.i = ashr i64 %10, 8
  br label %rope_size.exit

rope_size.exit:                                   ; preds = %entry, %sw.bb.i, %sw.bb3.i
  %retval.0.i = phi i64 [ %shr.i, %sw.bb3.i ], [ %7, %sw.bb.i ], [ 0, %entry ]
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %off) #6
  %cmp34 = icmp sgt i64 %retval.0.i, 0
  br i1 %cmp34, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond.cleanup6, %rope_size.exit
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %off) #6
  ret void

for.body:                                         ; preds = %rope_size.exit, %for.cond.cleanup6
  %i.035 = phi i64 [ %add, %for.cond.cleanup6 ], [ 0, %rope_size.exit ]
  %call1 = call ptr @unison_jit_rope_chunk_at(ptr noundef nonnull @unison_jit_BK, ptr noundef %r, i64 noundef %i.035, ptr noundef nonnull %off) #6
  %11 = ptrtoint ptr %call1 to i64
  %and.i.i21 = and i64 %11, -8
  %12 = inttoptr i64 %and.i.i21 to ptr
  %payload.i22 = getelementptr inbounds i8, ptr %12, i64 8
  %13 = load ptr, ptr %payload.i22, align 8
  %payload1.i = getelementptr inbounds i8, ptr %13, i64 16
  %14 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_BK, i64 76), align 4
  %idxprom.i.i23 = sext i32 %14 to i64
  %arrayidx.i.i24 = getelementptr inbounds [0 x ptr], ptr %payload.i22, i64 0, i64 %idxprom.i.i23
  %15 = load ptr, ptr %arrayidx.i.i24, align 8
  %16 = ptrtoint ptr %15 to i64
  %add.ptr.i = getelementptr inbounds i8, ptr %payload1.i, i64 %16
  %17 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_BK, i64 80), align 8
  %idxprom.i = sext i32 %17 to i64
  %arrayidx.i = getelementptr inbounds [0 x ptr], ptr %payload.i22, i64 0, i64 %idxprom.i
  %18 = load ptr, ptr %arrayidx.i, align 8
  %19 = ptrtoint ptr %18 to i64
  %cmp531 = icmp sgt ptr %18, null
  br i1 %cmp531, label %for.body7.lr.ph, label %for.cond.cleanup6

for.body7.lr.ph:                                  ; preds = %for.body
  %s.promoted = load i64, ptr %s, align 8
  br label %for.body7

for.cond.cleanup6:                                ; preds = %for.body7, %for.body
  %20 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_BK, i64 72), align 8
  %idxprom.i29 = sext i32 %20 to i64
  %arrayidx.i30 = getelementptr inbounds [0 x ptr], ptr %payload.i22, i64 0, i64 %idxprom.i29
  %21 = load ptr, ptr %arrayidx.i30, align 8
  %22 = ptrtoint ptr %21 to i64
  %add = add nsw i64 %i.035, %22
  %cmp = icmp slt i64 %add, %retval.0.i
  br i1 %cmp, label %for.body, label %for.cond.cleanup

for.body7:                                        ; preds = %for.body7.lr.ph, %for.body7
  %xor3.i.i33 = phi i64 [ %s.promoted, %for.body7.lr.ph ], [ %xor3.i.i, %for.body7 ]
  %k.032 = phi i64 [ 0, %for.body7.lr.ph ], [ %inc, %for.body7 ]
  %arrayidx = getelementptr inbounds i8, ptr %add.ptr.i, i64 %k.032
  %23 = load i8, ptr %arrayidx, align 1
  %conv = zext i8 %23 to i64
  %mul.i.i = mul i64 %conv, -4132994306676758123
  %shr.i.i = lshr i64 %mul.i.i, 47
  %xor.i.i = xor i64 %shr.i.i, %mul.i.i
  %mul1.i.i = mul i64 %xor.i.i, -4132994306676758123
  %mul2.i.i = mul i64 %xor3.i.i33, -4132994306676758123
  %xor3.i.i = xor i64 %mul1.i.i, %mul2.i.i
  store i64 %xor3.i.i, ptr %s, align 8
  %inc = add nuw nsw i64 %k.032, 1
  %exitcond.not = icmp eq i64 %inc, %19
  br i1 %exitcond.not, label %for.cond.cleanup6, label %for.body7
}

define internal fastcc ptr @deque_of(ptr noundef %list) #5 {
entry:
  %0 = ptrtoint ptr %list to i64
  %and.i = and i64 %0, 7
  %cmp.not = icmp eq i64 %and.i, 7
  br i1 %cmp.not, label %lor.lhs.false, label %return

lor.lhs.false:                                    ; preds = %entry
  %and.i20 = and i64 %0, -8
  %1 = inttoptr i64 %and.i20 to ptr
  %2 = load ptr, ptr %1, align 8
  %3 = ptrtoint ptr %2 to i64
  %4 = load i64, ptr @unison_jit_LF, align 8
  %cmp2.not = icmp eq i64 %4, %3
  br i1 %cmp2.not, label %if.end, label %return

if.end:                                           ; preds = %lor.lhs.false
  %payload = getelementptr inbounds i8, ptr %1, i64 8
  %5 = load ptr, ptr %payload, align 8
  %6 = ptrtoint ptr %5 to i64
  %and.i22 = and i64 %6, 7
  %7 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 16), align 8
  %cmp5.not = icmp eq i64 %and.i22, %7
  br i1 %cmp5.not, label %lor.lhs.false6, label %return

lor.lhs.false6:                                   ; preds = %if.end
  %and.i23 = and i64 %6, -8
  %8 = inttoptr i64 %and.i23 to ptr
  %9 = load ptr, ptr %8, align 8
  %10 = ptrtoint ptr %9 to i64
  %11 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 8), align 8
  %cmp10.not = icmp eq i64 %11, %10
  br i1 %cmp10.not, label %if.end12, label %return

if.end12:                                         ; preds = %lor.lhs.false6
  %payload14 = getelementptr inbounds i8, ptr %8, i64 8
  %12 = load ptr, ptr %payload14, align 8
  br label %return

return:                                           ; preds = %if.end12, %lor.lhs.false6, %if.end, %entry, %lor.lhs.false
  %retval.1 = phi ptr [ null, %lor.lhs.false ], [ null, %entry ], [ %12, %if.end12 ], [ null, %lor.lhs.false6 ], [ null, %if.end ]
  ret ptr %retval.1
}

declare void @unison_jit_lv_each(ptr noundef, i32 noundef, i32 noundef, ptr noundef, ptr noundef)

define internal void @mh_val_closure(ptr noundef %item, ptr noundef %env) #0 {
entry:
  %0 = ptrtoint ptr %item to i64
  %and.i = and i64 %0, 7
  %cmp.not = icmp eq i64 %and.i, 1
  br i1 %cmp.not, label %lor.lhs.false, label %if.then

lor.lhs.false:                                    ; preds = %entry
  %and.i12 = and i64 %0, -8
  %1 = inttoptr i64 %and.i12 to ptr
  %2 = load ptr, ptr %1, align 8
  %3 = ptrtoint ptr %2 to i64
  %4 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not = icmp eq i64 %4, %3
  br i1 %cmp2.not, label %if.end, label %if.then

if.then:                                          ; preds = %lor.lhs.false, %entry
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.6, ptr noundef %item) #6
  %ok = getelementptr inbounds i8, ptr %env, i64 8
  store i32 0, ptr %ok, align 8
  br label %cleanup

if.end:                                           ; preds = %lor.lhs.false
  %arrayidx = getelementptr inbounds i8, ptr %1, i64 16
  %5 = load ptr, ptr %arrayidx, align 8
  %6 = ptrtoint ptr %5 to i64
  %payload5 = getelementptr inbounds i8, ptr %1, i64 8
  %7 = load ptr, ptr %payload5, align 8
  tail call fastcc void @mh_val(ptr noundef %env, i64 noundef %6, ptr noundef %7)
  br label %cleanup

cleanup:                                          ; preds = %if.end, %if.then
  ret void
}

define internal fastcc ptr @wrapped_of(ptr noundef %x, ptr nocapture noundef readonly %w) #5 {
entry:
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_AK, i64 120), align 8
  %tobool.not.i = icmp eq i32 %0, 0
  br i1 %tobool.not.i, label %wrapped_word.exit, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %entry
  %1 = ptrtoint ptr %x to i64
  %and.i.i = and i64 %1, 7
  %cmp.not.i = icmp eq i64 %and.i.i, 7
  br i1 %cmp.not.i, label %lor.lhs.false1.i, label %wrapped_word.exit

lor.lhs.false1.i:                                 ; preds = %lor.lhs.false.i
  %and.i26.i = and i64 %1, -8
  %2 = inttoptr i64 %and.i26.i to ptr
  %3 = load ptr, ptr %2, align 8
  %4 = ptrtoint ptr %3 to i64
  %5 = load i64, ptr @unison_jit_LF, align 8
  %cmp3.not.i = icmp eq i64 %5, %4
  br i1 %cmp3.not.i, label %if.end.i, label %wrapped_word.exit

if.end.i:                                         ; preds = %lor.lhs.false1.i
  %payload.i = getelementptr inbounds i8, ptr %2, i64 8
  %6 = load ptr, ptr %payload.i, align 8
  %7 = ptrtoint ptr %6 to i64
  %and.i28.i = and i64 %7, 7
  %tag.i = getelementptr inbounds i8, ptr %w, i64 8
  %8 = load i64, ptr %tag.i, align 8
  %sext.i = shl i64 %8, 32
  %conv6.i = ashr exact i64 %sext.i, 32
  %cmp7.not.i = icmp eq i64 %and.i28.i, %conv6.i
  br i1 %cmp7.not.i, label %lor.lhs.false9.i, label %wrapped_word.exit

lor.lhs.false9.i:                                 ; preds = %if.end.i
  %and.i29.i = and i64 %7, -8
  %9 = inttoptr i64 %and.i29.i to ptr
  %10 = load ptr, ptr %9, align 8
  %11 = ptrtoint ptr %10 to i64
  %12 = load i64, ptr %w, align 8
  %cmp14.not.i = icmp eq i64 %12, %11
  br i1 %cmp14.not.i, label %if.end17.i, label %wrapped_word.exit

if.end17.i:                                       ; preds = %lor.lhs.false9.i
  %payload19.i = getelementptr inbounds i8, ptr %9, i64 8
  %13 = load ptr, ptr %payload19.i, align 8
  br label %wrapped_word.exit

wrapped_word.exit:                                ; preds = %entry, %lor.lhs.false.i, %lor.lhs.false1.i, %if.end.i, %lor.lhs.false9.i, %if.end17.i
  %retval.1.i = phi ptr [ null, %lor.lhs.false1.i ], [ null, %lor.lhs.false.i ], [ null, %entry ], [ %13, %if.end17.i ], [ null, %lor.lhs.false9.i ], [ null, %if.end.i ]
  ret ptr %retval.1.i
}

declare ptr @unison_jit_rope_chunk_at(ptr noundef, ptr noundef, i64 noundef, ptr noundef)

attributes #0 = { nounwind }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nofree nosync nounwind memory(readwrite, inaccessiblemem: none) }
attributes #4 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #5 = { inlinehint mustprogress nofree norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none) }
attributes #6 = { nounwind }
