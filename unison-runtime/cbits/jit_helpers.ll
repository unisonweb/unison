; Generated from jit_helpers.c by gen-helpers.sh. Do not edit.

%struct.EnvFacts = type { i64, i32, i32, i32, i32, i64, i64, i32, i32, ptr }
%struct.CmpFacts = type { i64, i64, i64, i64, i32 }
%struct.HashFacts = type { i64, i64, i64, i32 }
%struct.MH = type { i64, i32 }
%struct.ListFacts = type { i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, ptr, ptr, ptr }
%struct.RopeKind = type { i64, i64, i64, i64, i64, i64, ptr, i64, i64, i32, i32, i32, i32 }
%struct.ArrayFacts = type { %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, %struct.WrapInfo, ptr, i32 }
%struct.WrapInfo = type { i64, i64 }
%struct.UC = type { i32, i64 }
%struct.Fields = type { i64, i64, ptr, ptr, i32, { i64, i64 }, [2 x ptr] }

@unison_jit_EK = external global %struct.EnvFacts, align 8
@unison_jit_CK = external global %struct.CmpFacts, align 8
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
@.str.7 = private constant [16 x i8] c"type references\00", align 1
@.str.8 = private constant [20 x i8] c"function references\00", align 1
@.str.9 = private constant [28 x i8] c"text against something else\00", align 1
@.str.10 = private constant [29 x i8] c"bytes against something else\00", align 1
@__const.ue_closure.same = private constant [3 x ptr] [ptr getelementptr (i8, ptr @unison_jit_AK, i64 64), ptr @unison_jit_AK, ptr getelementptr (i8, ptr @unison_jit_AK, i64 32)], align 8

define { i64, i64 } @unison_jit_murmur(i64 noundef %u, ptr noundef %b) #0 {
entry:
  %s.i = alloca %struct.MH, align 8
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 24), align 8
  %tobool.not.i = icmp eq i32 %0, 0
  br i1 %tobool.not.i, label %mh_run.exit, label %if.end.i

if.end.i:                                         ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %s.i) #8
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
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %s.i) #8
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
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %found) #8
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_EK, i64 44), align 4
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %cleanup67, label %if.end

if.end:                                           ; preds = %entry
  %1 = ptrtoint ptr %henv to i64
  %and.i = and i64 %1, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then1, label %if.end3

if.then1:                                         ; preds = %if.end
  %call2 = tail call ptr @unison_jit_settle(ptr noundef %henv) #8
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
  %call20 = tail call ptr @unison_jit_settle(ptr noundef %8) #8
  br label %if.end21

if.end21:                                         ; preds = %if.then19, %if.end11
  %aenv.0 = phi ptr [ %call20, %if.then19 ], [ %8, %if.end11 ]
  %11 = ptrtoint ptr %9 to i64
  %and.i87 = and i64 %11, 7
  %cmp23 = icmp eq i64 %and.i87, 0
  br i1 %cmp23, label %if.then25, label %if.end27

if.then25:                                        ; preds = %if.end21
  %call26 = tail call ptr @unison_jit_settle(ptr noundef %9) #8
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
  %call46 = call ptr @unison_jit_settle(ptr noundef %14) #8
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
  %call65 = call ptr @unison_jit_settle(ptr noundef %24) #8
  br label %cleanup67

cleanup67:                                        ; preds = %land.lhs.true, %if.end36, %lor.lhs.false51, %if.end47, %if.end58, %cond.true, %if.end3, %lor.lhs.false, %entry
  %retval.1 = phi ptr [ null, %entry ], [ null, %lor.lhs.false ], [ null, %if.end3 ], [ null, %land.lhs.true ], [ null, %if.end36 ], [ null, %lor.lhs.false51 ], [ null, %if.end47 ], [ %call65, %cond.true ], [ %24, %if.end58 ]
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %found) #8
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

define { i64, i64 } @unison_jit_ucmp(i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2, i64 noundef %kinds) #0 {
entry:
  %s = alloca %struct.UC, align 8
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_CK, i64 32), align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %if.end

if.end:                                           ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %s) #8
  store i32 1, ptr %s, align 8
  %kinds2 = getelementptr inbounds i8, ptr %s, i64 8
  store i64 %kinds, ptr %kinds2, align 8
  %call = call fastcc i64 @uc_val(ptr noundef nonnull %s, i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2, i32 noundef 0)
  %1 = load i32, ptr %s, align 8
  %tobool4.not = icmp ne i32 %1, 0
  %. = zext i1 %tobool4.not to i64
  %.call = select i1 %tobool4.not, i64 %call, i64 0
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %s) #8
  br label %return

return:                                           ; preds = %entry, %if.end
  %retval.sroa.0.1 = phi i64 [ %., %if.end ], [ 0, %entry ]
  %retval.sroa.4.1 = phi i64 [ %.call, %if.end ], [ 0, %entry ]
  %.fca.0.insert = insertvalue { i64, i64 } poison, i64 %retval.sroa.0.1, 0
  %.fca.1.insert = insertvalue { i64, i64 } %.fca.0.insert, i64 %retval.sroa.4.1, 1
  ret { i64, i64 } %.fca.1.insert
}

define internal fastcc i64 @uc_val(ptr nocapture noundef %s, i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2, i32 noundef %tyeq) #0 {
entry:
  %f1.i = alloca %struct.Fields, align 8
  %f2.i = alloca %struct.Fields, align 8
  %f175.i = alloca %struct.Fields, align 8
  %f276.i = alloca %struct.Fields, align 8
  %0 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %if.end

if.end:                                           ; preds = %entry
  %1 = ptrtoint ptr %b1 to i64
  %and.i = and i64 %1, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then3, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %if.end
  %2 = ptrtoint ptr %b2 to i64
  %and.i42 = and i64 %2, 7
  %cmp2 = icmp eq i64 %and.i42, 0
  br i1 %cmp2, label %if.then3, label %if.end7

if.then3:                                         ; preds = %lor.lhs.false, %if.end
  %cond = phi ptr [ %b2, %lor.lhs.false ], [ %b1, %if.end ]
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str, ptr noundef %cond) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end7:                                          ; preds = %lor.lhs.false
  %and.i.i = and i64 %1, -8
  %3 = inttoptr i64 %and.i.i to ptr
  %4 = load ptr, ptr %3, align 8
  %5 = ptrtoint ptr %4 to i64
  %6 = load i64, ptr @unison_jit_HK, align 8
  %cmp.not.i = icmp eq i64 %6, %5
  br i1 %cmp.not.i, label %if.end.i, label %utag_kind.exit

if.end.i:                                         ; preds = %if.end7
  %payload.i = getelementptr inbounds i8, ptr %3, i64 8
  %7 = load ptr, ptr %payload.i, align 8
  %8 = ptrtoint ptr %7 to i64
  %9 = trunc i64 %8 to i32
  %conv.i = and i32 %9, 7
  br label %utag_kind.exit

utag_kind.exit:                                   ; preds = %if.end7, %if.end.i
  %retval.0.i = phi i32 [ %conv.i, %if.end.i ], [ 0, %if.end7 ]
  %and.i.i44 = and i64 %2, -8
  %10 = inttoptr i64 %and.i.i44 to ptr
  %11 = load ptr, ptr %10, align 8
  %12 = ptrtoint ptr %11 to i64
  %cmp.not.i45 = icmp eq i64 %6, %12
  br i1 %cmp.not.i45, label %if.end.i47, label %utag_kind.exit50

if.end.i47:                                       ; preds = %utag_kind.exit
  %payload.i48 = getelementptr inbounds i8, ptr %10, i64 8
  %13 = load ptr, ptr %payload.i48, align 8
  %14 = ptrtoint ptr %13 to i64
  %15 = trunc i64 %14 to i32
  %conv.i49 = and i32 %15, 7
  br label %utag_kind.exit50

utag_kind.exit50:                                 ; preds = %utag_kind.exit, %if.end.i47
  %retval.0.i46 = phi i32 [ %conv.i49, %if.end.i47 ], [ 0, %utag_kind.exit ]
  %tobool10 = icmp ne i32 %retval.0.i, 0
  %tobool11 = icmp ne i32 %retval.0.i46, 0
  %or.cond = select i1 %tobool10, i1 %tobool11, i1 false
  br i1 %or.cond, label %if.then12, label %if.end14

if.then12:                                        ; preds = %utag_kind.exit50
  %cmp.i = icmp eq i32 %retval.0.i, 3
  %cmp1.i = icmp eq i32 %retval.0.i46, 3
  %or.cond.i = and i1 %cmp.i, %cmp1.i
  br i1 %or.cond.i, label %if.then.i, label %if.end.i51

if.then.i:                                        ; preds = %if.then12
  %cmp.i.i = icmp slt i64 %u1, %u2
  %cmp1.i.i = icmp sgt i64 %u1, %u2
  %16 = zext i1 %cmp1.i.i to i64
  %cond2.i.i = select i1 %cmp.i.i, i64 -1, i64 %16
  br label %return

if.end.i51:                                       ; preds = %if.then12
  %cmp2.i = icmp eq i32 %retval.0.i, 4
  %cmp4.i = icmp eq i32 %retval.0.i46, 4
  %or.cond46.i = and i1 %cmp2.i, %cmp4.i
  br i1 %or.cond46.i, label %if.then5.i, label %if.end7.i

if.then5.i:                                       ; preds = %if.end.i51
  %cmp.i80.i = icmp ult i64 %u1, %u2
  %cmp1.i81.i = icmp ugt i64 %u1, %u2
  %17 = zext i1 %cmp1.i81.i to i64
  %cond2.i82.i = select i1 %cmp.i80.i, i64 -1, i64 %17
  br label %return

if.end7.i:                                        ; preds = %if.end.i51
  %or.cond47.i = and i1 %cmp2.i, %cmp1.i
  br i1 %or.cond47.i, label %if.then11.i, label %if.end14.i

if.then11.i:                                      ; preds = %if.end7.i
  %cmp12.i = icmp slt i64 %u2, 0
  br i1 %cmp12.i, label %return, label %cond.false.i

cond.false.i:                                     ; preds = %if.then11.i
  %cmp.i83.i = icmp ult i64 %u1, %u2
  %cmp1.i84.i = icmp ugt i64 %u1, %u2
  %18 = zext i1 %cmp1.i84.i to i64
  %cond2.i85.i = select i1 %cmp.i83.i, i64 -1, i64 %18
  br label %return

if.end14.i:                                       ; preds = %if.end7.i
  %or.cond48.i = and i1 %cmp.i, %cmp4.i
  br i1 %or.cond48.i, label %if.then18.i, label %if.end25.i

if.then18.i:                                      ; preds = %if.end14.i
  %cmp19.i = icmp slt i64 %u1, 0
  br i1 %cmp19.i, label %return, label %cond.false21.i

cond.false21.i:                                   ; preds = %if.then18.i
  %cmp.i86.i = icmp ult i64 %u1, %u2
  %cmp1.i87.i = icmp ugt i64 %u1, %u2
  %19 = zext i1 %cmp1.i87.i to i64
  %cond2.i88.i = select i1 %cmp.i86.i, i64 -1, i64 %19
  br label %return

if.end25.i:                                       ; preds = %if.end14.i
  %cmp26.i = icmp eq i32 %retval.0.i, 2
  %cmp28.i = icmp eq i32 %retval.0.i46, 2
  %or.cond49.i = and i1 %cmp26.i, %cmp28.i
  br i1 %or.cond49.i, label %if.then29.i, label %if.end38.i

if.then29.i:                                      ; preds = %if.end25.i
  %cmp30.i = icmp sgt i64 %u1, 0
  %cmp31.i = icmp sgt i64 %u2, 0
  %or.cond50.i = or i1 %cmp30.i, %cmp31.i
  br i1 %or.cond50.i, label %if.then32.i, label %if.end34.i

if.then32.i:                                      ; preds = %if.then29.i
  %cmp.i89.i = icmp slt i64 %u1, %u2
  %cmp1.i90.i = icmp sgt i64 %u1, %u2
  %20 = zext i1 %cmp1.i90.i to i64
  %cond2.i91.i = select i1 %cmp.i89.i, i64 -1, i64 %20
  br label %return

if.end34.i:                                       ; preds = %if.then29.i
  %and.i52 = and i64 %u2, 9223372036854775807
  %and35.i = and i64 %u1, 9223372036854775807
  %cmp.i92.i = icmp ult i64 %and.i52, %and35.i
  %cmp1.i93.i = icmp ugt i64 %and.i52, %and35.i
  %21 = zext i1 %cmp1.i93.i to i64
  %cond2.i94.i = select i1 %cmp.i92.i, i64 -1, i64 %21
  br label %return

if.end38.i:                                       ; preds = %if.end25.i
  %tobool.not.i = icmp eq i32 %tyeq, 0
  %cmp40.not.i = icmp eq i32 %retval.0.i, %retval.0.i46
  %or.cond79.i = or i1 %tobool.not.i, %cmp40.not.i
  br i1 %or.cond79.i, label %if.end44.i, label %if.then41.i

if.then41.i:                                      ; preds = %if.end38.i
  %cmp.i95.i = icmp ult i32 %retval.0.i, %retval.0.i46
  %cmp1.i96.i = icmp ugt i32 %retval.0.i, %retval.0.i46
  %22 = zext i1 %cmp1.i96.i to i64
  %cond2.i97.i = select i1 %cmp.i95.i, i64 -1, i64 %22
  br label %return

if.end44.i:                                       ; preds = %if.end38.i
  %cmp.i98.i = icmp slt i64 %u1, %u2
  %cmp1.i99.i = icmp sgt i64 %u1, %u2
  %23 = zext i1 %cmp1.i99.i to i64
  %cond2.i100.i = select i1 %cmp.i98.i, i64 -1, i64 %23
  br label %return

if.end14:                                         ; preds = %utag_kind.exit50
  %brmerge = select i1 %tobool10, i1 true, i1 %tobool11
  %.mux = select i1 %tobool10, i64 -1, i64 1
  br i1 %brmerge, label %return, label %if.end20

if.end20:                                         ; preds = %if.end14
  %cmp21 = icmp eq ptr %b1, %b2
  br i1 %cmp21, label %return, label %if.end23

if.end23:                                         ; preds = %if.end20
  %24 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp.i91 = icmp eq i64 %24, %5
  %25 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp2.i92 = icmp eq i64 %25, %5
  %or.cond.i93 = select i1 %cmp.i91, i1 true, i1 %cmp2.i92
  %26 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp4.i94 = icmp eq i64 %26, %5
  %or.cond21.i95 = select i1 %or.cond.i93, i1 true, i1 %cmp4.i94
  %27 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 16), align 8
  %cmp6.i96 = icmp eq i64 %27, %5
  %or.cond22.i97 = select i1 %or.cond21.i95, i1 true, i1 %cmp6.i96
  br i1 %or.cond22.i97, label %uc_kind.exit106, label %if.end.i98

if.end.i98:                                       ; preds = %if.end23
  %28 = load i64, ptr @unison_jit_LF, align 8
  %cmp7.i99 = icmp eq i64 %28, %5
  br i1 %cmp7.i99, label %uc_kind.exit106, label %if.end9.i100

if.end9.i100:                                     ; preds = %if.end.i98
  %29 = load i64, ptr @unison_jit_CK, align 8
  %cmp10.not.i101 = icmp ne i64 %29, 0
  %cmp11.i102 = icmp eq i64 %29, %5
  %or.cond23.i103 = and i1 %cmp10.not.i101, %cmp11.i102
  br i1 %or.cond23.i103, label %uc_kind.exit106, label %if.end13.i104

if.end13.i104:                                    ; preds = %if.end9.i100
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.5, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %uc_kind.exit106

uc_kind.exit106:                                  ; preds = %if.end23, %if.end.i98, %if.end9.i100, %if.end13.i104
  %retval.0.i105 = phi i32 [ -1, %if.end13.i104 ], [ 1, %if.end23 ], [ 3, %if.end.i98 ], [ 0, %if.end9.i100 ]
  %30 = load i32, ptr %s, align 8
  %tobool.not.i53 = icmp eq i32 %30, 0
  br i1 %tobool.not.i53, label %cond.end.i, label %cond.true.i

cond.true.i:                                      ; preds = %uc_kind.exit106
  %31 = load ptr, ptr %10, align 8
  %32 = ptrtoint ptr %31 to i64
  %33 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp.i84 = icmp eq i64 %33, %32
  %34 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp2.i85 = icmp eq i64 %34, %32
  %or.cond.i86 = select i1 %cmp.i84, i1 true, i1 %cmp2.i85
  %35 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp4.i87 = icmp eq i64 %35, %32
  %or.cond21.i = select i1 %or.cond.i86, i1 true, i1 %cmp4.i87
  %36 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 16), align 8
  %cmp6.i = icmp eq i64 %36, %32
  %or.cond22.i = select i1 %or.cond21.i, i1 true, i1 %cmp6.i
  br i1 %or.cond22.i, label %cond.end.i, label %if.end.i88

if.end.i88:                                       ; preds = %cond.true.i
  %37 = load i64, ptr @unison_jit_LF, align 8
  %cmp7.i = icmp eq i64 %37, %32
  br i1 %cmp7.i, label %cond.end.i, label %if.end9.i

if.end9.i:                                        ; preds = %if.end.i88
  %38 = load i64, ptr @unison_jit_CK, align 8
  %cmp10.not.i = icmp ne i64 %38, 0
  %cmp11.i = icmp eq i64 %38, %32
  %or.cond23.i = and i1 %cmp10.not.i, %cmp11.i
  br i1 %or.cond23.i, label %cond.end.i, label %if.end13.i

if.end13.i:                                       ; preds = %if.end9.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.5, ptr noundef %b2) #8
  store i32 0, ptr %s, align 8
  br label %cond.end.i

cond.end.i:                                       ; preds = %if.end13.i, %if.end9.i, %if.end.i88, %cond.true.i, %uc_kind.exit106
  %cond.i = phi i32 [ -1, %uc_kind.exit106 ], [ -1, %if.end13.i ], [ 1, %cond.true.i ], [ 3, %if.end.i88 ], [ 0, %if.end9.i ]
  %39 = load i32, ptr %s, align 8
  %tobool3.not.i = icmp eq i32 %39, 0
  br i1 %tobool3.not.i, label %return, label %if.end.i54

if.end.i54:                                       ; preds = %cond.end.i
  %cmp.not.i55 = icmp eq i32 %retval.0.i105, %cond.i
  br i1 %cmp.not.i55, label %if.end7.i57, label %if.then4.i

if.then4.i:                                       ; preds = %if.end.i54
  %cmp.i80 = icmp slt i32 %retval.0.i105, %cond.i
  %cmp1.i81 = icmp sgt i32 %retval.0.i105, %cond.i
  %40 = zext i1 %cmp1.i81 to i64
  %cond2.i82 = select i1 %cmp.i80, i64 -1, i64 %40
  br label %return

if.end7.i57:                                      ; preds = %if.end.i54
  switch i32 %retval.0.i105, label %if.end100.i [
    i32 1, label %if.then10.i
    i32 0, label %if.then42.i
  ]

if.then10.i:                                      ; preds = %if.end7.i57
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f1.i) #8
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f2.i) #8
  %call11.i = call fastcc i32 @uc_fields(ptr noundef nonnull %s, ptr noundef %b1, ptr noundef nonnull %f1.i)
  %tobool12.not.i = icmp eq i32 %call11.i, 0
  br i1 %tobool12.not.i, label %cleanup37.i, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %if.then10.i
  %call13.i = call fastcc i32 @uc_fields(ptr noundef nonnull %s, ptr noundef %b2, ptr noundef nonnull %f2.i)
  %tobool14.not.i = icmp eq i32 %call13.i, 0
  br i1 %tobool14.not.i, label %cleanup37.i, label %if.end16.i

if.end16.i:                                       ; preds = %lor.lhs.false.i
  %tobool17.i = icmp ne i32 %tyeq, 0
  br i1 %tobool17.i, label %land.lhs.true.i, label %if.end23.i

land.lhs.true.i:                                  ; preds = %if.end16.i
  %41 = load i64, ptr %f1.i, align 8
  %42 = load i64, ptr %f2.i, align 8
  %cmp19.not.i = icmp eq i64 %41, %42
  br i1 %cmp19.not.i, label %if.end23.i, label %if.then21.i

if.then21.i:                                      ; preds = %land.lhs.true.i
  call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.7, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %cleanup37.i

if.end23.i:                                       ; preds = %land.lhs.true.i, %if.end16.i
  %43 = load i64, ptr %f1.i, align 8
  %and.i59 = and i64 %43, 65535
  %44 = load i64, ptr %f2.i, align 8
  %and26.i = and i64 %44, 65535
  %cmp.i77 = icmp ult i64 %and.i59, %and26.i
  %cmp1.i78 = icmp ugt i64 %and.i59, %and26.i
  %45 = zext i1 %cmp1.i78 to i64
  %cond2.i79 = select i1 %cmp.i77, i64 -1, i64 %45
  %cmp28.not.i = icmp eq i64 %cond2.i79, 0
  br i1 %cmp28.not.i, label %if.end31.i, label %cleanup37.i

if.end31.i:                                       ; preds = %if.end23.i
  %shr.i = lshr i64 %43, 16
  %46 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_CK, i64 24), align 8
  %cmp34.i = icmp eq i64 %shr.i, %46
  %47 = select i1 %tobool17.i, i1 true, i1 %cmp34.i
  %lor.ext.i = zext i1 %47 to i32
  %call36.i = call fastcc i64 @uc_fieldwise(ptr noundef nonnull %s, ptr noundef nonnull %f1.i, ptr noundef nonnull %f2.i, i32 noundef %lor.ext.i)
  br label %cleanup37.i

cleanup37.i:                                      ; preds = %if.end31.i, %if.end23.i, %if.then21.i, %lor.lhs.false.i, %if.then10.i
  %retval.1.i60 = phi i64 [ 0, %if.then21.i ], [ 0, %lor.lhs.false.i ], [ 0, %if.then10.i ], [ %call36.i, %if.end31.i ], [ %cond2.i79, %if.end23.i ]
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f2.i) #8
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f1.i) #8
  br label %return

if.then42.i:                                      ; preds = %if.end7.i57
  %payload.i58 = getelementptr inbounds i8, ptr %3, i64 8
  %48 = load ptr, ptr %payload.i58, align 8
  %payload45.i = getelementptr inbounds i8, ptr %10, i64 8
  %49 = load ptr, ptr %payload45.i, align 8
  %cmp47.not.i = icmp eq ptr %48, %49
  br i1 %cmp47.not.i, label %if.end51.i, label %if.then49.i

if.then49.i:                                      ; preds = %if.then42.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.8, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end51.i:                                       ; preds = %if.then42.i
  %arrayidx55.i = getelementptr inbounds i8, ptr %3, i64 40
  %50 = load ptr, ptr %arrayidx55.i, align 8
  %arrayidx58.i = getelementptr inbounds i8, ptr %10, i64 40
  %51 = load ptr, ptr %arrayidx58.i, align 8
  %cmp.i70 = icmp ult ptr %50, %51
  %cmp1.i71 = icmp ugt ptr %50, %51
  %52 = zext i1 %cmp1.i71 to i64
  %cond2.i72 = select i1 %cmp.i70, i64 -1, i64 %52
  %cmp60.not.i = icmp eq i64 %cond2.i72, 0
  br i1 %cmp60.not.i, label %if.end63.i, label %return

if.end63.i:                                       ; preds = %if.end51.i
  %arrayidx66.i = getelementptr inbounds i8, ptr %3, i64 48
  %53 = load ptr, ptr %arrayidx66.i, align 8
  %arrayidx69.i = getelementptr inbounds i8, ptr %10, i64 48
  %54 = load ptr, ptr %arrayidx69.i, align 8
  %cmp.i66 = icmp ult ptr %53, %54
  %cmp1.i67 = icmp ugt ptr %53, %54
  %55 = zext i1 %cmp1.i67 to i64
  %cond2.i = select i1 %cmp.i66, i64 -1, i64 %55
  %cmp71.not.i = icmp eq i64 %cond2.i, 0
  br i1 %cmp71.not.i, label %if.end74.i, label %return

if.end74.i:                                       ; preds = %if.end63.i
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f175.i) #8
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f276.i) #8
  %arrayidx79.i = getelementptr inbounds i8, ptr %3, i64 24
  %56 = load ptr, ptr %arrayidx79.i, align 8
  %arrayidx82.i = getelementptr inbounds i8, ptr %3, i64 32
  %57 = load ptr, ptr %arrayidx82.i, align 8
  %call83.i = call fastcc i32 @uc_segs(ptr noundef nonnull %s, ptr noundef %56, ptr noundef %57, ptr noundef nonnull %f175.i)
  %tobool84.not.i = icmp eq i32 %call83.i, 0
  br i1 %tobool84.not.i, label %cleanup97.i, label %lor.lhs.false85.i

lor.lhs.false85.i:                                ; preds = %if.end74.i
  %arrayidx88.i = getelementptr inbounds i8, ptr %10, i64 24
  %58 = load ptr, ptr %arrayidx88.i, align 8
  %arrayidx91.i = getelementptr inbounds i8, ptr %10, i64 32
  %59 = load ptr, ptr %arrayidx91.i, align 8
  %call92.i = call fastcc i32 @uc_segs(ptr noundef nonnull %s, ptr noundef %58, ptr noundef %59, ptr noundef nonnull %f276.i)
  %tobool93.not.i = icmp eq i32 %call92.i, 0
  br i1 %tobool93.not.i, label %cleanup97.i, label %if.end95.i

if.end95.i:                                       ; preds = %lor.lhs.false85.i
  %call96.i = call fastcc i64 @uc_fieldwise(ptr noundef nonnull %s, ptr noundef nonnull %f175.i, ptr noundef nonnull %f276.i, i32 noundef %tyeq)
  br label %cleanup97.i

cleanup97.i:                                      ; preds = %if.end95.i, %lor.lhs.false85.i, %if.end74.i
  %retval.2.i = phi i64 [ %call96.i, %if.end95.i ], [ 0, %lor.lhs.false85.i ], [ 0, %if.end74.i ]
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f276.i) #8
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f175.i) #8
  br label %return

if.end100.i:                                      ; preds = %if.end7.i57
  %call101.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_TK, ptr noundef %b1)
  %call102.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_TK, ptr noundef %b2)
  %cmp103.i = icmp ne ptr %call101.i, null
  %cmp106.i = icmp ne ptr %call102.i, null
  %or.cond.i61 = select i1 %cmp103.i, i1 true, i1 %cmp106.i
  br i1 %or.cond.i61, label %if.then108.i, label %if.end127.i

if.then108.i:                                     ; preds = %if.end100.i
  %or.cond203.i = select i1 %cmp103.i, i1 %cmp106.i, i1 false
  br i1 %or.cond203.i, label %land.lhs.true114.i, label %if.end119.i

land.lhs.true114.i:                               ; preds = %if.then108.i
  %kinds.i = getelementptr inbounds i8, ptr %s, i64 8
  %60 = load i64, ptr %kinds.i, align 8
  %and115.i = and i64 %60, 1
  %tobool116.not.i = icmp eq i64 %and115.i, 0
  br i1 %tobool116.not.i, label %if.end119.i, label %if.then117.i

if.then117.i:                                     ; preds = %land.lhs.true114.i
  %call118.i = tail call i64 @unison_jit_foreign_cmp(ptr noundef %b1, ptr noundef %b2, i64 noundef 1) #8
  br label %return

if.end119.i:                                      ; preds = %land.lhs.true114.i, %if.then108.i
  %cond125.i = select i1 %cmp103.i, ptr %b2, ptr %b1
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.9, ptr noundef %cond125.i) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end127.i:                                      ; preds = %if.end100.i
  %call128.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_BK, ptr noundef %b1)
  %call129.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_BK, ptr noundef %b2)
  %cmp130.i = icmp ne ptr %call128.i, null
  %cmp133.i = icmp ne ptr %call129.i, null
  %or.cond204.i = select i1 %cmp130.i, i1 true, i1 %cmp133.i
  br i1 %or.cond204.i, label %if.then135.i, label %if.end155.i

if.then135.i:                                     ; preds = %if.end127.i
  %or.cond205.i = select i1 %cmp130.i, i1 %cmp133.i, i1 false
  br i1 %or.cond205.i, label %land.lhs.true141.i, label %if.end147.i

land.lhs.true141.i:                               ; preds = %if.then135.i
  %kinds142.i = getelementptr inbounds i8, ptr %s, i64 8
  %61 = load i64, ptr %kinds142.i, align 8
  %and143.i = and i64 %61, 2
  %tobool144.not.i = icmp eq i64 %and143.i, 0
  br i1 %tobool144.not.i, label %if.end147.i, label %if.then145.i

if.then145.i:                                     ; preds = %land.lhs.true141.i
  %call146.i = tail call i64 @unison_jit_foreign_cmp(ptr noundef %b1, ptr noundef %b2, i64 noundef 2) #8
  br label %return

if.end147.i:                                      ; preds = %land.lhs.true141.i, %if.then135.i
  %cond153.i = select i1 %cmp130.i, ptr %b2, ptr %b1
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.10, ptr noundef %cond153.i) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end155.i:                                      ; preds = %if.end127.i
  %call156.i = tail call fastcc ptr @deque_of(ptr noundef %b1)
  %call157.i = tail call fastcc ptr @deque_of(ptr noundef %b2)
  %cmp158.i = icmp ne ptr %call156.i, null
  %cmp161.i = icmp ne ptr %call157.i, null
  %or.cond206.i = select i1 %cmp158.i, i1 %cmp161.i, i1 false
  br i1 %or.cond206.i, label %if.then163.i, label %if.end165.i

if.then163.i:                                     ; preds = %if.end155.i
  %call164.i = tail call fastcc i64 @uc_lists(ptr noundef nonnull %s, ptr noundef nonnull %call156.i, ptr noundef nonnull %call157.i, i32 noundef %tyeq)
  br label %return

if.end165.i:                                      ; preds = %if.end155.i
  %62 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_AK, i64 120), align 8
  %tobool166.not.i = icmp eq i32 %62, 0
  br i1 %tobool166.not.i, label %if.end192.i, label %if.then167.i

if.then167.i:                                     ; preds = %if.end165.i
  %call168.i = tail call fastcc ptr @wrapped_of(ptr noundef %b1, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 16))
  %call169.i = tail call fastcc ptr @wrapped_of(ptr noundef %b2, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 16))
  %cmp170.i = icmp ne ptr %call168.i, null
  %cmp173.i = icmp ne ptr %call169.i, null
  %or.cond207.i = select i1 %cmp170.i, i1 %cmp173.i, i1 false
  br i1 %or.cond207.i, label %if.then175.i, label %if.end177.i

if.then175.i:                                     ; preds = %if.then167.i
  %call176.i = tail call fastcc i64 @uc_arrays(ptr noundef nonnull %s, ptr noundef nonnull %call168.i, ptr noundef nonnull %call169.i, i32 noundef %tyeq)
  br label %cleanup190.i

if.end177.i:                                      ; preds = %if.then167.i
  %call178.i = tail call fastcc ptr @wrapped_of(ptr noundef %b1, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 48))
  %call179.i = tail call fastcc ptr @wrapped_of(ptr noundef %b2, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 48))
  %cmp180.i = icmp eq ptr %call178.i, null
  %cmp183.i = icmp eq ptr %call179.i, null
  %or.cond208.not.i = select i1 %cmp180.i, i1 true, i1 %cmp183.i
  br i1 %or.cond208.not.i, label %cleanup190.i, label %if.then185.i

if.then185.i:                                     ; preds = %if.end177.i
  %call186.i = tail call fastcc i64 @uc_barrays(ptr noundef nonnull %call178.i, ptr noundef nonnull %call179.i)
  br label %cleanup190.i

cleanup190.i:                                     ; preds = %if.then185.i, %if.end177.i, %if.then175.i
  %cleanup.dest.slot.1.i = phi i1 [ false, %if.then175.i ], [ %or.cond208.not.i, %if.end177.i ], [ false, %if.then185.i ]
  %retval.5.i = phi i64 [ %call176.i, %if.then175.i ], [ undef, %if.end177.i ], [ %call186.i, %if.then185.i ]
  br i1 %cleanup.dest.slot.1.i, label %if.end192.i, label %return

if.end192.i:                                      ; preds = %cleanup190.i, %if.end165.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.4, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %return

return:                                           ; preds = %if.end192.i, %cleanup190.i, %if.then163.i, %if.end147.i, %if.then145.i, %if.end119.i, %if.then117.i, %cleanup97.i, %if.end63.i, %if.end51.i, %if.then49.i, %cleanup37.i, %if.then4.i, %cond.end.i, %if.end44.i, %if.then41.i, %if.end34.i, %if.then32.i, %cond.false21.i, %if.then18.i, %cond.false.i, %if.then11.i, %if.then5.i, %if.then.i, %if.end14, %if.end20, %entry, %if.then3
  %retval.1 = phi i64 [ 0, %if.then3 ], [ 0, %entry ], [ %.mux, %if.end14 ], [ 0, %if.end20 ], [ %cond2.i.i, %if.then.i ], [ %cond2.i82.i, %if.then5.i ], [ %cond2.i97.i, %if.then41.i ], [ %cond2.i100.i, %if.end44.i ], [ %cond2.i85.i, %cond.false.i ], [ 1, %if.then11.i ], [ %cond2.i88.i, %cond.false21.i ], [ -1, %if.then18.i ], [ %cond2.i91.i, %if.then32.i ], [ %cond2.i94.i, %if.end34.i ], [ %cond2.i82, %if.then4.i ], [ %retval.1.i60, %cleanup37.i ], [ 0, %if.then49.i ], [ 0, %cond.end.i ], [ %retval.2.i, %cleanup97.i ], [ %cond2.i72, %if.end51.i ], [ %cond2.i, %if.end63.i ], [ %call118.i, %if.then117.i ], [ 0, %if.end119.i ], [ %call146.i, %if.then145.i ], [ 0, %if.end147.i ], [ %call164.i, %if.then163.i ], [ 0, %if.end192.i ], [ %retval.5.i, %cleanup190.i ]
  ret i64 %retval.1
}

define { i64, i64 } @unison_jit_ueq(i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2, i64 noundef %kinds) #0 {
entry:
  %s = alloca %struct.UC, align 8
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_CK, i64 32), align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %if.end

if.end:                                           ; preds = %entry
  call void @llvm.lifetime.start.p0(i64 16, ptr nonnull %s) #8
  store i32 1, ptr %s, align 8
  %kinds2 = getelementptr inbounds i8, ptr %s, i64 8
  store i64 %kinds, ptr %kinds2, align 8
  %call = call fastcc i32 @ue_val(ptr noundef nonnull %s, i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2)
  %1 = load i32, ptr %s, align 8
  %tobool4.not = icmp ne i32 %1, 0
  %retval.sroa.0.0 = zext i1 %tobool4.not to i64
  %narrow = select i1 %tobool4.not, i32 %call, i32 0
  %retval.sroa.4.0 = sext i32 %narrow to i64
  call void @llvm.lifetime.end.p0(i64 16, ptr nonnull %s) #8
  br label %return

return:                                           ; preds = %entry, %if.end
  %retval.sroa.0.1 = phi i64 [ %retval.sroa.0.0, %if.end ], [ 0, %entry ]
  %retval.sroa.4.1 = phi i64 [ %retval.sroa.4.0, %if.end ], [ 0, %entry ]
  %.fca.0.insert = insertvalue { i64, i64 } poison, i64 %retval.sroa.0.1, 0
  %.fca.1.insert = insertvalue { i64, i64 } %.fca.0.insert, i64 %retval.sroa.4.1, 1
  ret { i64, i64 } %.fca.1.insert
}

define internal fastcc i32 @ue_val(ptr nocapture noundef %s, i64 noundef %u1, ptr noundef %b1, i64 noundef %u2, ptr noundef %b2) #0 {
entry:
  %f1.i = alloca %struct.Fields, align 8
  %f2.i = alloca %struct.Fields, align 8
  %f170.i = alloca %struct.Fields, align 8
  %f271.i = alloca %struct.Fields, align 8
  %0 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %if.end

if.end:                                           ; preds = %entry
  %1 = ptrtoint ptr %b1 to i64
  %and.i = and i64 %1, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then3, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %if.end
  %2 = ptrtoint ptr %b2 to i64
  %and.i56 = and i64 %2, 7
  %cmp2 = icmp eq i64 %and.i56, 0
  br i1 %cmp2, label %if.then3, label %if.end7

if.then3:                                         ; preds = %lor.lhs.false, %if.end
  %cond = phi ptr [ %b2, %lor.lhs.false ], [ %b1, %if.end ]
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str, ptr noundef %cond) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end7:                                          ; preds = %lor.lhs.false
  %and.i.i = and i64 %1, -8
  %3 = inttoptr i64 %and.i.i to ptr
  %4 = load ptr, ptr %3, align 8
  %5 = ptrtoint ptr %4 to i64
  %6 = load i64, ptr @unison_jit_HK, align 8
  %cmp.not.i = icmp eq i64 %6, %5
  br i1 %cmp.not.i, label %if.end.i, label %utag_kind.exit

if.end.i:                                         ; preds = %if.end7
  %payload.i = getelementptr inbounds i8, ptr %3, i64 8
  %7 = load ptr, ptr %payload.i, align 8
  %8 = ptrtoint ptr %7 to i64
  %9 = trunc i64 %8 to i32
  %conv.i = and i32 %9, 7
  br label %utag_kind.exit

utag_kind.exit:                                   ; preds = %if.end7, %if.end.i
  %retval.0.i = phi i32 [ %conv.i, %if.end.i ], [ 0, %if.end7 ]
  %and.i.i58 = and i64 %2, -8
  %10 = inttoptr i64 %and.i.i58 to ptr
  %11 = load ptr, ptr %10, align 8
  %12 = ptrtoint ptr %11 to i64
  %cmp.not.i59 = icmp eq i64 %6, %12
  br i1 %cmp.not.i59, label %if.end.i61, label %utag_kind.exit64

if.end.i61:                                       ; preds = %utag_kind.exit
  %payload.i62 = getelementptr inbounds i8, ptr %10, i64 8
  %13 = load ptr, ptr %payload.i62, align 8
  %14 = ptrtoint ptr %13 to i64
  %15 = trunc i64 %14 to i32
  %conv.i63 = and i32 %15, 7
  br label %utag_kind.exit64

utag_kind.exit64:                                 ; preds = %utag_kind.exit, %if.end.i61
  %retval.0.i60 = phi i32 [ %conv.i63, %if.end.i61 ], [ 0, %utag_kind.exit ]
  %tobool10 = icmp ne i32 %retval.0.i, 0
  %tobool11 = icmp ne i32 %retval.0.i60, 0
  %or.cond = select i1 %tobool10, i1 %tobool11, i1 false
  br i1 %or.cond, label %if.then12, label %if.end23

if.then12:                                        ; preds = %utag_kind.exit64
  %cmp13 = icmp eq i32 %retval.0.i, %retval.0.i60
  br i1 %cmp13, label %land.rhs, label %lor.lhs.false14

lor.lhs.false14:                                  ; preds = %if.then12
  %cmp15 = icmp eq i32 %retval.0.i, 3
  %cmp17 = icmp eq i32 %retval.0.i60, 4
  %or.cond34 = select i1 %cmp15, i1 %cmp17, i1 false
  br i1 %or.cond34, label %land.rhs, label %lor.lhs.false18

lor.lhs.false18:                                  ; preds = %lor.lhs.false14
  %cmp19 = icmp eq i32 %retval.0.i, 4
  %cmp21 = icmp eq i32 %retval.0.i60, 3
  %or.cond35 = select i1 %cmp19, i1 %cmp21, i1 false
  br i1 %or.cond35, label %land.rhs, label %return

land.rhs:                                         ; preds = %lor.lhs.false18, %lor.lhs.false14, %if.then12
  %cmp22 = icmp eq i64 %u1, %u2
  %16 = zext i1 %cmp22 to i32
  br label %return

if.end23:                                         ; preds = %utag_kind.exit64
  %or.cond36 = select i1 %tobool10, i1 true, i1 %tobool11
  br i1 %or.cond36, label %return, label %if.end28

if.end28:                                         ; preds = %if.end23
  %cmp29 = icmp eq ptr %b1, %b2
  br i1 %cmp29, label %return, label %if.end31

if.end31:                                         ; preds = %if.end28
  %17 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp.i91 = icmp eq i64 %17, %5
  %18 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp2.i92 = icmp eq i64 %18, %5
  %or.cond.i93 = select i1 %cmp.i91, i1 true, i1 %cmp2.i92
  %19 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp4.i94 = icmp eq i64 %19, %5
  %or.cond21.i95 = select i1 %or.cond.i93, i1 true, i1 %cmp4.i94
  %20 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 16), align 8
  %cmp6.i96 = icmp eq i64 %20, %5
  %or.cond22.i97 = select i1 %or.cond21.i95, i1 true, i1 %cmp6.i96
  br i1 %or.cond22.i97, label %uc_kind.exit107, label %if.end.i98

if.end.i98:                                       ; preds = %if.end31
  %21 = load i64, ptr @unison_jit_LF, align 8
  %cmp7.i99 = icmp eq i64 %21, %5
  br i1 %cmp7.i99, label %uc_kind.exit107, label %if.end9.i100

if.end9.i100:                                     ; preds = %if.end.i98
  %22 = load i64, ptr @unison_jit_CK, align 8
  %cmp10.not.i101 = icmp ne i64 %22, 0
  %cmp11.i102 = icmp eq i64 %22, %5
  %or.cond23.i103 = and i1 %cmp10.not.i101, %cmp11.i102
  br i1 %or.cond23.i103, label %uc_kind.exit107, label %if.end13.i104

if.end13.i104:                                    ; preds = %if.end9.i100
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.5, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %uc_kind.exit107

uc_kind.exit107:                                  ; preds = %if.end31, %if.end.i98, %if.end9.i100, %if.end13.i104
  %retval.0.i106 = phi i32 [ -1, %if.end13.i104 ], [ 1, %if.end31 ], [ 3, %if.end.i98 ], [ 0, %if.end9.i100 ]
  %23 = load i32, ptr %s, align 8
  %tobool.not.i = icmp eq i32 %23, 0
  br i1 %tobool.not.i, label %cond.end.i, label %cond.true.i

cond.true.i:                                      ; preds = %uc_kind.exit107
  %24 = load ptr, ptr %10, align 8
  %25 = ptrtoint ptr %24 to i64
  %26 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp.i = icmp eq i64 %26, %25
  %27 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp2.i = icmp eq i64 %27, %25
  %or.cond.i85 = select i1 %cmp.i, i1 true, i1 %cmp2.i
  %28 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp4.i = icmp eq i64 %28, %25
  %or.cond21.i = select i1 %or.cond.i85, i1 true, i1 %cmp4.i
  %29 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 16), align 8
  %cmp6.i = icmp eq i64 %29, %25
  %or.cond22.i = select i1 %or.cond21.i, i1 true, i1 %cmp6.i
  br i1 %or.cond22.i, label %cond.end.i, label %if.end.i86

if.end.i86:                                       ; preds = %cond.true.i
  %30 = load i64, ptr @unison_jit_LF, align 8
  %cmp7.i = icmp eq i64 %30, %25
  br i1 %cmp7.i, label %cond.end.i, label %if.end9.i

if.end9.i:                                        ; preds = %if.end.i86
  %31 = load i64, ptr @unison_jit_CK, align 8
  %cmp10.not.i = icmp ne i64 %31, 0
  %cmp11.i = icmp eq i64 %31, %25
  %or.cond23.i = and i1 %cmp10.not.i, %cmp11.i
  br i1 %or.cond23.i, label %cond.end.i, label %if.end13.i87

if.end13.i87:                                     ; preds = %if.end9.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.5, ptr noundef %b2) #8
  store i32 0, ptr %s, align 8
  br label %cond.end.i

cond.end.i:                                       ; preds = %if.end13.i87, %if.end9.i, %if.end.i86, %cond.true.i, %uc_kind.exit107
  %cond.i = phi i32 [ -1, %uc_kind.exit107 ], [ -1, %if.end13.i87 ], [ 1, %cond.true.i ], [ 3, %if.end.i86 ], [ 0, %if.end9.i ]
  %32 = load i32, ptr %s, align 8
  %tobool3.not.i = icmp ne i32 %32, 0
  %cmp.not.i65 = icmp eq i32 %retval.0.i106, %cond.i
  %or.cond436.i = and i1 %cmp.not.i65, %tobool3.not.i
  br i1 %or.cond436.i, label %if.end5.i, label %return

if.end5.i:                                        ; preds = %cond.end.i
  switch i32 %retval.0.i106, label %if.end94.i [
    i32 1, label %if.then7.i
    i32 0, label %if.then44.i
  ]

if.then7.i:                                       ; preds = %if.end5.i
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f1.i) #8
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f2.i) #8
  %call8.i = call fastcc i32 @uc_fields(ptr noundef nonnull %s, ptr noundef %b1, ptr noundef nonnull %f1.i)
  %tobool9.not.i = icmp eq i32 %call8.i, 0
  br i1 %tobool9.not.i, label %cleanup.i, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %if.then7.i
  %call10.i = call fastcc i32 @uc_fields(ptr noundef nonnull %s, ptr noundef %b2, ptr noundef nonnull %f2.i)
  %tobool11.not.i = icmp eq i32 %call10.i, 0
  br i1 %tobool11.not.i, label %cleanup.i, label %if.end13.i

if.end13.i:                                       ; preds = %lor.lhs.false.i
  %n.i = getelementptr inbounds i8, ptr %f1.i, i64 8
  %33 = load i64, ptr %n.i, align 8
  %cmp14.i = icmp eq i64 %33, 1
  %n15.i = getelementptr inbounds i8, ptr %f2.i, i64 8
  %34 = load i64, ptr %n15.i, align 8
  %cmp16.i = icmp eq i64 %34, 1
  %or.cond.i = select i1 %cmp14.i, i1 %cmp16.i, i1 false
  %35 = load i64, ptr %f1.i, align 8
  %36 = load i64, ptr %f2.i, align 8
  %cmp19.i = icmp eq i64 %35, %36
  br i1 %or.cond.i, label %if.then17.i, label %if.else.i

if.then17.i:                                      ; preds = %if.end13.i
  br i1 %cmp19.i, label %if.end39.i, label %lor.lhs.false20.i

lor.lhs.false20.i:                                ; preds = %if.then17.i
  %37 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_CK, i64 8), align 8
  %cmp22.i = icmp eq i64 %35, %37
  %38 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_CK, i64 16), align 8
  %cmp25.i = icmp eq i64 %36, %38
  %or.cond435.i = select i1 %cmp22.i, i1 %cmp25.i, i1 false
  br i1 %or.cond435.i, label %if.end39.i, label %lor.lhs.false26.i

lor.lhs.false26.i:                                ; preds = %lor.lhs.false20.i
  %cmp28.i = icmp eq i64 %35, %38
  %cmp31.i = icmp eq i64 %36, %37
  %or.cond108 = and i1 %cmp31.i, %cmp28.i
  br i1 %or.cond108, label %if.end39.i, label %cleanup.i

if.else.i:                                        ; preds = %if.end13.i
  br i1 %cmp19.i, label %if.end39.i, label %cleanup.i

if.end39.i:                                       ; preds = %lor.lhs.false26.i, %if.else.i, %lor.lhs.false20.i, %if.then17.i
  %call40.i = call fastcc i32 @ue_fieldwise(ptr noundef nonnull %s, ptr noundef nonnull %f1.i, ptr noundef nonnull %f2.i)
  br label %cleanup.i

cleanup.i:                                        ; preds = %if.end39.i, %if.else.i, %lor.lhs.false26.i, %lor.lhs.false.i, %if.then7.i
  %retval.0.i67 = phi i32 [ %call40.i, %if.end39.i ], [ 0, %lor.lhs.false.i ], [ 0, %if.then7.i ], [ 0, %lor.lhs.false26.i ], [ 0, %if.else.i ]
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f2.i) #8
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f1.i) #8
  br label %return

if.then44.i:                                      ; preds = %if.end5.i
  %payload.i66 = getelementptr inbounds i8, ptr %3, i64 8
  %39 = load ptr, ptr %payload.i66, align 8
  %payload47.i = getelementptr inbounds i8, ptr %10, i64 8
  %40 = load ptr, ptr %payload47.i, align 8
  %cmp49.not.i = icmp eq ptr %39, %40
  br i1 %cmp49.not.i, label %if.end52.i, label %if.then50.i

if.then50.i:                                      ; preds = %if.then44.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.8, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end52.i:                                       ; preds = %if.then44.i
  %arrayidx55.i = getelementptr inbounds i8, ptr %3, i64 40
  %41 = load ptr, ptr %arrayidx55.i, align 8
  %arrayidx58.i = getelementptr inbounds i8, ptr %10, i64 40
  %42 = load ptr, ptr %arrayidx58.i, align 8
  %cmp59.not.i = icmp eq ptr %41, %42
  br i1 %cmp59.not.i, label %lor.lhs.false60.i, label %return

lor.lhs.false60.i:                                ; preds = %if.end52.i
  %arrayidx63.i = getelementptr inbounds i8, ptr %3, i64 48
  %43 = load ptr, ptr %arrayidx63.i, align 8
  %arrayidx66.i = getelementptr inbounds i8, ptr %10, i64 48
  %44 = load ptr, ptr %arrayidx66.i, align 8
  %cmp67.not.i = icmp eq ptr %43, %44
  br i1 %cmp67.not.i, label %if.end69.i, label %return

if.end69.i:                                       ; preds = %lor.lhs.false60.i
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f170.i) #8
  call void @llvm.lifetime.start.p0(i64 72, ptr nonnull %f271.i) #8
  %arrayidx74.i = getelementptr inbounds i8, ptr %3, i64 24
  %45 = load ptr, ptr %arrayidx74.i, align 8
  %arrayidx77.i = getelementptr inbounds i8, ptr %3, i64 32
  %46 = load ptr, ptr %arrayidx77.i, align 8
  %call78.i = call fastcc i32 @uc_segs(ptr noundef nonnull %s, ptr noundef %45, ptr noundef %46, ptr noundef nonnull %f170.i)
  %tobool79.not.i = icmp eq i32 %call78.i, 0
  br i1 %tobool79.not.i, label %cleanup92.i, label %lor.lhs.false80.i

lor.lhs.false80.i:                                ; preds = %if.end69.i
  %arrayidx83.i = getelementptr inbounds i8, ptr %10, i64 24
  %47 = load ptr, ptr %arrayidx83.i, align 8
  %arrayidx86.i = getelementptr inbounds i8, ptr %10, i64 32
  %48 = load ptr, ptr %arrayidx86.i, align 8
  %call87.i = call fastcc i32 @uc_segs(ptr noundef nonnull %s, ptr noundef %47, ptr noundef %48, ptr noundef nonnull %f271.i)
  %tobool88.not.i = icmp eq i32 %call87.i, 0
  br i1 %tobool88.not.i, label %cleanup92.i, label %if.end90.i

if.end90.i:                                       ; preds = %lor.lhs.false80.i
  %call91.i = call fastcc i32 @ue_fieldwise(ptr noundef nonnull %s, ptr noundef nonnull %f170.i, ptr noundef nonnull %f271.i)
  br label %cleanup92.i

cleanup92.i:                                      ; preds = %if.end90.i, %lor.lhs.false80.i, %if.end69.i
  %retval.1.i = phi i32 [ %call91.i, %if.end90.i ], [ 0, %lor.lhs.false80.i ], [ 0, %if.end69.i ]
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f271.i) #8
  call void @llvm.lifetime.end.p0(i64 72, ptr nonnull %f170.i) #8
  br label %return

if.end94.i:                                       ; preds = %if.end5.i
  %call95.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_TK, ptr noundef %b1)
  %call96.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_TK, ptr noundef %b2)
  %cmp97.i = icmp ne ptr %call95.i, null
  %cmp99.i = icmp ne ptr %call96.i, null
  %or.cond327.i = select i1 %cmp97.i, i1 true, i1 %cmp99.i
  br i1 %or.cond327.i, label %if.then100.i, label %if.end117.i

if.then100.i:                                     ; preds = %if.end94.i
  %or.cond328.i = select i1 %cmp97.i, i1 %cmp99.i, i1 false
  br i1 %or.cond328.i, label %land.lhs.true104.i, label %if.end109.i

land.lhs.true104.i:                               ; preds = %if.then100.i
  %kinds.i = getelementptr inbounds i8, ptr %s, i64 8
  %49 = load i64, ptr %kinds.i, align 8
  %and.i68 = and i64 %49, 1
  %tobool105.not.i = icmp eq i64 %and.i68, 0
  br i1 %tobool105.not.i, label %if.end109.i, label %if.then106.i

if.then106.i:                                     ; preds = %land.lhs.true104.i
  %call107.i = tail call i64 @unison_jit_foreign_eq(ptr noundef %b1, ptr noundef %b2, i64 noundef 1) #8
  %cmp108.i = icmp eq i64 %call107.i, 1
  %conv.i69 = zext i1 %cmp108.i to i32
  br label %return

if.end109.i:                                      ; preds = %land.lhs.true104.i, %if.then100.i
  %cond115.i = select i1 %cmp97.i, ptr %b2, ptr %b1
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.9, ptr noundef %cond115.i) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end117.i:                                      ; preds = %if.end94.i
  %call118.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_BK, ptr noundef %b1)
  %call119.i = tail call fastcc ptr @rope_of(ptr noundef nonnull @unison_jit_BK, ptr noundef %b2)
  %cmp120.i = icmp ne ptr %call118.i, null
  %cmp123.i = icmp ne ptr %call119.i, null
  %or.cond329.i = select i1 %cmp120.i, i1 true, i1 %cmp123.i
  br i1 %or.cond329.i, label %if.then125.i, label %if.end147.i

if.then125.i:                                     ; preds = %if.end117.i
  %or.cond330.i = select i1 %cmp120.i, i1 %cmp123.i, i1 false
  br i1 %or.cond330.i, label %land.lhs.true131.i, label %if.end139.i

land.lhs.true131.i:                               ; preds = %if.then125.i
  %kinds132.i = getelementptr inbounds i8, ptr %s, i64 8
  %50 = load i64, ptr %kinds132.i, align 8
  %and133.i = and i64 %50, 2
  %tobool134.not.i = icmp eq i64 %and133.i, 0
  br i1 %tobool134.not.i, label %if.end139.i, label %if.then135.i

if.then135.i:                                     ; preds = %land.lhs.true131.i
  %call136.i = tail call i64 @unison_jit_foreign_eq(ptr noundef %b1, ptr noundef %b2, i64 noundef 2) #8
  %cmp137.i = icmp eq i64 %call136.i, 1
  %conv138.i = zext i1 %cmp137.i to i32
  br label %return

if.end139.i:                                      ; preds = %land.lhs.true131.i, %if.then125.i
  %cond145.i = select i1 %cmp120.i, ptr %b2, ptr %b1
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.10, ptr noundef %cond145.i) #8
  store i32 0, ptr %s, align 8
  br label %return

if.end147.i:                                      ; preds = %if.end117.i
  %call148.i = tail call fastcc ptr @deque_of(ptr noundef %b1)
  %call149.i = tail call fastcc ptr @deque_of(ptr noundef %b2)
  %cmp150.i = icmp ne ptr %call148.i, null
  %cmp153.i = icmp ne ptr %call149.i, null
  %or.cond331.i = select i1 %cmp150.i, i1 %cmp153.i, i1 false
  br i1 %or.cond331.i, label %if.then155.i, label %if.end195.i

if.then155.i:                                     ; preds = %if.end147.i
  %51 = ptrtoint ptr %call148.i to i64
  %and.i73 = and i64 %51, 7
  %cmp157.i = icmp eq i64 %and.i73, 2
  br i1 %cmp157.i, label %cond.true159.i, label %cond.end164.i

cond.true159.i:                                   ; preds = %if.then155.i
  %and.i72 = and i64 %51, -8
  %52 = inttoptr i64 %and.i72 to ptr
  %arrayidx162.i = getelementptr inbounds i8, ptr %52, i64 32
  %53 = load ptr, ptr %arrayidx162.i, align 8
  %54 = ptrtoint ptr %53 to i64
  %shr.i = ashr i64 %54, 8
  br label %cond.end164.i

cond.end164.i:                                    ; preds = %cond.true159.i, %if.then155.i
  %cond165.i = phi i64 [ %shr.i, %cond.true159.i ], [ 0, %if.then155.i ]
  %55 = ptrtoint ptr %call149.i to i64
  %and.i71 = and i64 %55, 7
  %cmp167.i = icmp eq i64 %and.i71, 2
  br i1 %cmp167.i, label %cond.true169.i, label %cond.end175.i

cond.true169.i:                                   ; preds = %cond.end164.i
  %and.i70 = and i64 %55, -8
  %56 = inttoptr i64 %and.i70 to ptr
  %arrayidx172.i = getelementptr inbounds i8, ptr %56, i64 32
  %57 = load ptr, ptr %arrayidx172.i, align 8
  %58 = ptrtoint ptr %57 to i64
  %shr173.i = ashr i64 %58, 8
  br label %cond.end175.i

cond.end175.i:                                    ; preds = %cond.true169.i, %cond.end164.i
  %cond176.i = phi i64 [ %shr173.i, %cond.true169.i ], [ 0, %cond.end164.i ]
  %cmp177.not.i = icmp eq i64 %cond165.i, %cond176.i
  br i1 %cmp177.not.i, label %for.cond.i.preheader, label %return

for.cond.i.preheader:                             ; preds = %cond.end175.i
  %cmp181.i118 = icmp sgt i64 %cond165.i, 0
  br i1 %cmp181.i118, label %land.rhs.i, label %for.end.critedge.i

for.cond.i:                                       ; preds = %for.body.i
  %inc.i = add nuw nsw i64 %i.0.i119, 1
  %exitcond123.not = icmp eq i64 %inc.i, %cond165.i
  br i1 %exitcond123.not, label %for.end.critedge.i, label %land.rhs.i

land.rhs.i:                                       ; preds = %for.cond.i.preheader, %for.cond.i
  %i.0.i119 = phi i64 [ %inc.i, %for.cond.i ], [ 0, %for.cond.i.preheader ]
  %59 = load i32, ptr %s, align 8
  %tobool184.not.i = icmp eq i32 %59, 0
  br i1 %tobool184.not.i, label %for.end.critedge.i, label %for.body.i

for.body.i:                                       ; preds = %land.rhs.i
  %call185.i = tail call ptr @unison_jit_list_at(ptr noundef nonnull %call148.i, i64 noundef %i.0.i119) #8
  %call186.i = tail call ptr @unison_jit_list_at(ptr noundef nonnull %call149.i, i64 noundef %i.0.i119) #8
  %call187.i = tail call fastcc i32 @ue_elems(ptr noundef nonnull %s, ptr noundef %call185.i, ptr noundef %call186.i)
  %tobool188.not.i = icmp eq i32 %call187.i, 0
  br i1 %tobool188.not.i, label %return, label %for.cond.i

for.end.critedge.i:                               ; preds = %for.cond.i, %land.rhs.i, %for.cond.i.preheader
  %60 = load i32, ptr %s, align 8
  br label %return

if.end195.i:                                      ; preds = %if.end147.i
  %61 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_AK, i64 120), align 8
  %tobool196.not.i = icmp eq i32 %61, 0
  br i1 %tobool196.not.i, label %if.end311.i, label %if.then197.i

if.then197.i:                                     ; preds = %if.end195.i
  %call198.i = tail call fastcc ptr @wrapped_of(ptr noundef %b1, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 16))
  %call199.i = tail call fastcc ptr @wrapped_of(ptr noundef %b2, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 16))
  %cmp200.i = icmp ne ptr %call198.i, null
  %cmp203.i = icmp ne ptr %call199.i, null
  %or.cond332.i = select i1 %cmp200.i, i1 %cmp203.i, i1 false
  br i1 %or.cond332.i, label %if.then205.i, label %if.end236.i

if.then205.i:                                     ; preds = %if.then197.i
  %ptrs.i = getelementptr inbounds i8, ptr %call198.i, i64 8
  %62 = load i64, ptr %ptrs.i, align 8
  %ptrs206.i = getelementptr inbounds i8, ptr %call199.i, i64 8
  %63 = load i64, ptr %ptrs206.i, align 8
  %cmp207.not.i = icmp eq i64 %62, %63
  br i1 %cmp207.not.i, label %for.cond212.i.preheader, label %cleanup307.i

for.cond212.i.preheader:                          ; preds = %if.then205.i
  %cmp214.i116.not = icmp eq i64 %62, 0
  br i1 %cmp214.i116.not, label %for.end234.critedge.i, label %land.rhs216.i.lr.ph

land.rhs216.i.lr.ph:                              ; preds = %for.cond212.i.preheader
  %payload222.i = getelementptr inbounds i8, ptr %call198.i, i64 24
  %payload224.i = getelementptr inbounds i8, ptr %call199.i, i64 24
  br label %land.rhs216.i

for.cond212.i:                                    ; preds = %for.body221.i
  %inc231.i = add nuw i64 %i211.0.i117, 1
  %64 = load i64, ptr %ptrs.i, align 8
  %cmp214.i = icmp ult i64 %inc231.i, %64
  br i1 %cmp214.i, label %land.rhs216.i, label %for.end234.critedge.i

land.rhs216.i:                                    ; preds = %land.rhs216.i.lr.ph, %for.cond212.i
  %i211.0.i117 = phi i64 [ 0, %land.rhs216.i.lr.ph ], [ %inc231.i, %for.cond212.i ]
  %65 = load i32, ptr %s, align 8
  %tobool218.not.i = icmp eq i32 %65, 0
  br i1 %tobool218.not.i, label %for.end234.critedge.i, label %for.body221.i

for.body221.i:                                    ; preds = %land.rhs216.i
  %arrayidx223.i = getelementptr inbounds [0 x ptr], ptr %payload222.i, i64 0, i64 %i211.0.i117
  %66 = load ptr, ptr %arrayidx223.i, align 8
  %arrayidx225.i = getelementptr inbounds [0 x ptr], ptr %payload224.i, i64 0, i64 %i211.0.i117
  %67 = load ptr, ptr %arrayidx225.i, align 8
  %call226.i = tail call fastcc i32 @ue_elems(ptr noundef nonnull %s, ptr noundef %66, ptr noundef %67)
  %tobool227.not.i = icmp eq i32 %call226.i, 0
  br i1 %tobool227.not.i, label %cleanup307.i, label %for.cond212.i

for.end234.critedge.i:                            ; preds = %for.cond212.i, %land.rhs216.i, %for.cond212.i.preheader
  %68 = load i32, ptr %s, align 8
  br label %cleanup307.i

if.end236.i:                                      ; preds = %if.then197.i
  %call237.i = tail call fastcc ptr @wrapped_of(ptr noundef %b1, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 48))
  %call238.i = tail call fastcc ptr @wrapped_of(ptr noundef %b2, ptr noundef nonnull getelementptr inbounds (i8, ptr @unison_jit_AK, i64 48))
  %cmp239.i = icmp ne ptr %call237.i, null
  %cmp242.i = icmp ne ptr %call238.i, null
  %or.cond333.i = select i1 %cmp239.i, i1 %cmp242.i, i1 false
  br i1 %or.cond333.i, label %if.then244.i, label %for.body281.i

if.then244.i:                                     ; preds = %if.end236.i
  %bytes.i = getelementptr inbounds i8, ptr %call237.i, i64 8
  %69 = load i64, ptr %bytes.i, align 8
  %bytes245.i = getelementptr inbounds i8, ptr %call238.i, i64 8
  %70 = load i64, ptr %bytes245.i, align 8
  %cmp246.not.i = icmp eq i64 %69, %70
  br i1 %cmp246.not.i, label %if.end249.i, label %cleanup307.i

if.end249.i:                                      ; preds = %if.then244.i
  %payload250.i = getelementptr inbounds i8, ptr %call237.i, i64 16
  %payload251.i = getelementptr inbounds i8, ptr %call238.i, i64 16
  %cmp256.not.i112 = icmp eq i64 %69, 0
  br i1 %cmp256.not.i112, label %cleanup270.i, label %for.body259.i.preheader

for.body259.i.preheader:                          ; preds = %if.end249.i
  %71 = load i8, ptr %payload250.i, align 1
  %72 = load i8, ptr %payload251.i, align 1
  %cmp264.not.i125 = icmp eq i8 %71, %72
  br i1 %cmp264.not.i125, label %for.cond254.i, label %cleanup270.i

for.cond254.i:                                    ; preds = %for.body259.i.preheader, %for.body259.i
  %i253.0.i113126 = phi i64 [ %inc269.i, %for.body259.i ], [ 0, %for.body259.i.preheader ]
  %inc269.i = add nuw i64 %i253.0.i113126, 1
  %exitcond122 = icmp eq i64 %inc269.i, %69
  br i1 %exitcond122, label %cleanup270.i.loopexit, label %for.body259.i

for.body259.i:                                    ; preds = %for.cond254.i
  %arrayidx260.i = getelementptr inbounds i8, ptr %payload250.i, i64 %inc269.i
  %73 = load i8, ptr %arrayidx260.i, align 1
  %arrayidx262.i = getelementptr inbounds i8, ptr %payload251.i, i64 %inc269.i
  %74 = load i8, ptr %arrayidx262.i, align 1
  %cmp264.not.i = icmp eq i8 %73, %74
  br i1 %cmp264.not.i, label %for.cond254.i, label %cleanup270.i.loopexit

cleanup270.i.loopexit:                            ; preds = %for.cond254.i, %for.body259.i
  %cmp256.not.i.le = icmp uge i64 %inc269.i, %69
  br label %cleanup270.i

cleanup270.i:                                     ; preds = %cleanup270.i.loopexit, %for.body259.i.preheader, %if.end249.i
  %cmp256.not.i.lcssa = phi i1 [ %cmp256.not.i112, %if.end249.i ], [ %cmp256.not.i112, %for.body259.i.preheader ], [ %cmp256.not.i.le, %cleanup270.i.loopexit ]
  %spec.select.i = zext i1 %cmp256.not.i.lcssa to i32
  br label %cleanup307.i

for.cond277.i:                                    ; preds = %for.body281.i
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond = icmp eq i64 %indvars.iv.next, 3
  br i1 %exitcond, label %cleanup307.i.loopexit129.split.loop.exit, label %for.body281.i

for.body281.i:                                    ; preds = %if.end236.i, %for.cond277.i
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.cond277.i ], [ 0, %if.end236.i ]
  %retval.7.i111 = phi i32 [ %retval.8.i, %for.cond277.i ], [ undef, %if.end236.i ]
  %arrayidx282.i = getelementptr inbounds [3 x ptr], ptr @__const.ue_closure.same, i64 0, i64 %indvars.iv
  %75 = load ptr, ptr %arrayidx282.i, align 8
  %call283.i = tail call fastcc i64 @wrapped_word(ptr noundef %b1, ptr noundef %75)
  %call286.i = tail call fastcc i64 @wrapped_word(ptr noundef %b2, ptr noundef %75)
  %cmp287.i = icmp eq i64 %call283.i, 0
  %cmp290.i = icmp eq i64 %call286.i, 0
  %or.cond334.not.i = select i1 %cmp287.i, i1 true, i1 %cmp290.i
  %cmp293.i = icmp eq i64 %call283.i, %call286.i
  %conv294.i = zext i1 %cmp293.i to i32
  %retval.8.i = select i1 %or.cond334.not.i, i32 %retval.7.i111, i32 %conv294.i
  br i1 %or.cond334.not.i, label %for.cond277.i, label %cleanup307.i

cleanup307.i.loopexit129.split.loop.exit:         ; preds = %for.cond277.i
  %cmp278.i.le = icmp ugt i64 %indvars.iv, 1
  br label %cleanup307.i

cleanup307.i:                                     ; preds = %cleanup307.i.loopexit129.split.loop.exit, %for.body281.i, %for.body221.i, %cleanup270.i, %if.then244.i, %for.end234.critedge.i, %if.then205.i
  %cleanup.dest.slot.7.i = phi i1 [ false, %for.end234.critedge.i ], [ false, %if.then205.i ], [ false, %cleanup270.i ], [ false, %if.then244.i ], [ false, %for.body221.i ], [ %cmp278.i.le, %cleanup307.i.loopexit129.split.loop.exit ], [ false, %for.body281.i ]
  %retval.11.i = phi i32 [ %68, %for.end234.critedge.i ], [ 0, %if.then205.i ], [ %spec.select.i, %cleanup270.i ], [ 0, %if.then244.i ], [ 0, %for.body221.i ], [ %retval.8.i, %for.body281.i ], [ %retval.8.i, %cleanup307.i.loopexit129.split.loop.exit ]
  br i1 %cleanup.dest.slot.7.i, label %if.end311.i, label %return

if.end311.i:                                      ; preds = %cleanup307.i, %if.end195.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.4, ptr noundef %b1) #8
  store i32 0, ptr %s, align 8
  br label %return

return:                                           ; preds = %for.body.i, %if.end311.i, %cleanup307.i, %for.end.critedge.i, %cond.end175.i, %if.end139.i, %if.then135.i, %if.end109.i, %if.then106.i, %cleanup92.i, %lor.lhs.false60.i, %if.end52.i, %if.then50.i, %cleanup.i, %cond.end.i, %land.rhs, %lor.lhs.false18, %if.end23, %if.end28, %entry, %if.then3
  %retval.1 = phi i32 [ 0, %if.then3 ], [ 0, %entry ], [ 0, %lor.lhs.false18 ], [ %16, %land.rhs ], [ 0, %if.end23 ], [ 1, %if.end28 ], [ %retval.0.i67, %cleanup.i ], [ 0, %if.then50.i ], [ %retval.1.i, %cleanup92.i ], [ 0, %cond.end.i ], [ 0, %lor.lhs.false60.i ], [ 0, %if.end52.i ], [ %conv.i69, %if.then106.i ], [ 0, %if.end109.i ], [ %conv138.i, %if.then135.i ], [ 0, %if.end139.i ], [ 0, %if.end311.i ], [ %retval.11.i, %cleanup307.i ], [ %60, %for.end.critedge.i ], [ 0, %cond.end175.i ], [ 0, %for.body.i ]
  ret i32 %retval.1
}

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
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str, ptr noundef %b.tr) #8
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
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.1, ptr noundef %b.tr) #8
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
  %call51.i = tail call ptr @unison_jit_settle(ptr noundef %32) #8
  %arrayidx54.i = getelementptr inbounds i8, ptr %2, i64 24
  %33 = load ptr, ptr %arrayidx54.i, align 8
  %call55.i = tail call ptr @unison_jit_settle(ptr noundef %33) #8
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
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.3, ptr noundef %cond.i) #8
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
  tail call void @unison_jit_lv_each(ptr noundef nonnull %call102.i, i32 noundef 1, i32 noundef 0, ptr noundef nonnull @mh_val_closure, ptr noundef nonnull %s) #8
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
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.4, ptr noundef %b.tr) #8
  store i32 0, ptr %ok, align 8
  br label %return

if.else160.i:                                     ; preds = %if.else88.i
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.5, ptr noundef %b.tr) #8
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
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %off) #8
  %cmp107 = icmp sgt i64 %retval.0.i, 0
  br i1 %cmp107, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %while.end, %rope_size.exit
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %off) #8
  ret void

for.body:                                         ; preds = %rope_size.exit, %while.end
  %i.0108 = phi i64 [ %add, %while.end ], [ 0, %rope_size.exit ]
  %call1 = call ptr @unison_jit_rope_chunk_at(ptr noundef nonnull @unison_jit_TK, ptr noundef %r, i64 noundef %i.0108, ptr noundef nonnull %off) #8
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
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %off) #8
  %cmp34 = icmp sgt i64 %retval.0.i, 0
  br i1 %cmp34, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond.cleanup6, %rope_size.exit
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %off) #8
  ret void

for.body:                                         ; preds = %rope_size.exit, %for.cond.cleanup6
  %i.035 = phi i64 [ %add, %for.cond.cleanup6 ], [ 0, %rope_size.exit ]
  %call1 = call ptr @unison_jit_rope_chunk_at(ptr noundef nonnull @unison_jit_BK, ptr noundef %r, i64 noundef %i.035, ptr noundef nonnull %off) #8
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
  tail call void @unison_jit_murmur_fail(ptr noundef nonnull @.str.6, ptr noundef %item) #8
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

define internal fastcc i64 @wrapped_word(ptr noundef %x, ptr nocapture noundef readonly %w) #5 {
entry:
  %0 = load i32, ptr getelementptr inbounds (i8, ptr @unison_jit_AK, i64 120), align 8
  %tobool.not = icmp eq i32 %0, 0
  br i1 %tobool.not, label %return, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %1 = ptrtoint ptr %x to i64
  %and.i = and i64 %1, 7
  %cmp.not = icmp eq i64 %and.i, 7
  br i1 %cmp.not, label %lor.lhs.false1, label %return

lor.lhs.false1:                                   ; preds = %lor.lhs.false
  %and.i26 = and i64 %1, -8
  %2 = inttoptr i64 %and.i26 to ptr
  %3 = load ptr, ptr %2, align 8
  %4 = ptrtoint ptr %3 to i64
  %5 = load i64, ptr @unison_jit_LF, align 8
  %cmp3.not = icmp eq i64 %5, %4
  br i1 %cmp3.not, label %if.end, label %return

if.end:                                           ; preds = %lor.lhs.false1
  %payload = getelementptr inbounds i8, ptr %2, i64 8
  %6 = load ptr, ptr %payload, align 8
  %7 = ptrtoint ptr %6 to i64
  %and.i28 = and i64 %7, 7
  %tag = getelementptr inbounds i8, ptr %w, i64 8
  %8 = load i64, ptr %tag, align 8
  %sext = shl i64 %8, 32
  %conv6 = ashr exact i64 %sext, 32
  %cmp7.not = icmp eq i64 %and.i28, %conv6
  br i1 %cmp7.not, label %lor.lhs.false9, label %return

lor.lhs.false9:                                   ; preds = %if.end
  %and.i29 = and i64 %7, -8
  %9 = inttoptr i64 %and.i29 to ptr
  %10 = load ptr, ptr %9, align 8
  %11 = ptrtoint ptr %10 to i64
  %12 = load i64, ptr %w, align 8
  %cmp14.not = icmp eq i64 %12, %11
  br i1 %cmp14.not, label %if.end17, label %return

if.end17:                                         ; preds = %lor.lhs.false9
  %payload19 = getelementptr inbounds i8, ptr %9, i64 8
  %13 = load ptr, ptr %payload19, align 8
  %14 = ptrtoint ptr %13 to i64
  br label %return

return:                                           ; preds = %if.end17, %lor.lhs.false9, %if.end, %entry, %lor.lhs.false, %lor.lhs.false1
  %retval.1 = phi i64 [ 0, %lor.lhs.false1 ], [ 0, %lor.lhs.false ], [ 0, %entry ], [ %14, %if.end17 ], [ 0, %lor.lhs.false9 ], [ 0, %if.end ]
  ret i64 %retval.1
}

declare void @unison_jit_cmp_fail(ptr noundef, ptr noundef)

define internal fastcc range(i32 0, 2) i32 @uc_fields(ptr nocapture noundef writeonly %s, ptr noundef %c, ptr noundef %f) #0 {
entry:
  %0 = ptrtoint ptr %c to i64
  %and.i = and i64 %0, -8
  %1 = inttoptr i64 %and.i to ptr
  %2 = load ptr, ptr %1, align 8
  %3 = ptrtoint ptr %2 to i64
  %uw = getelementptr inbounds i8, ptr %f, i64 40
  %us = getelementptr inbounds i8, ptr %f, i64 16
  store ptr %uw, ptr %us, align 8
  %bp = getelementptr inbounds i8, ptr %f, i64 56
  %bs = getelementptr inbounds i8, ptr %f, i64 24
  store ptr %bp, ptr %bs, align 8
  %rev = getelementptr inbounds i8, ptr %f, i64 32
  store i32 0, ptr %rev, align 8
  %4 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_HK, i64 8), align 8
  %cmp = icmp eq i64 %4, %3
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  %arrayidx = getelementptr inbounds i8, ptr %1, i64 16
  %5 = load ptr, ptr %arrayidx, align 8
  %6 = ptrtoint ptr %5 to i64
  store i64 %6, ptr %f, align 8
  %n = getelementptr inbounds i8, ptr %f, i64 8
  store i64 0, ptr %n, align 8
  br label %cleanup

if.end:                                           ; preds = %entry
  %7 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 72), align 8
  %cmp4 = icmp eq i64 %7, %3
  br i1 %cmp4, label %if.then5, label %if.end21

if.then5:                                         ; preds = %if.end
  %arrayidx8 = getelementptr inbounds i8, ptr %1, i64 24
  %8 = load ptr, ptr %arrayidx8, align 8
  %9 = ptrtoint ptr %8 to i64
  store i64 %9, ptr %f, align 8
  %n10 = getelementptr inbounds i8, ptr %f, i64 8
  store i64 1, ptr %n10, align 8
  %arrayidx13 = getelementptr inbounds i8, ptr %1, i64 32
  %10 = load ptr, ptr %arrayidx13, align 8
  %11 = ptrtoint ptr %10 to i64
  store i64 %11, ptr %uw, align 8
  %arrayidx18 = getelementptr inbounds i8, ptr %1, i64 16
  %12 = load ptr, ptr %arrayidx18, align 8
  store ptr %12, ptr %bp, align 8
  br label %cleanup

if.end21:                                         ; preds = %if.end
  %13 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 80), align 8
  %cmp22 = icmp eq i64 %13, %3
  %arrayidx26 = getelementptr inbounds i8, ptr %1, i64 32
  %14 = load ptr, ptr %arrayidx26, align 8
  %15 = ptrtoint ptr %14 to i64
  store i64 %15, ptr %f, align 8
  br i1 %cmp22, label %if.then23, label %if.end49

if.then23:                                        ; preds = %if.end21
  %n28 = getelementptr inbounds i8, ptr %f, i64 8
  store i64 2, ptr %n28, align 8
  %arrayidx31 = getelementptr inbounds i8, ptr %1, i64 40
  %16 = load ptr, ptr %arrayidx31, align 8
  %17 = ptrtoint ptr %16 to i64
  store i64 %17, ptr %uw, align 8
  %arrayidx36 = getelementptr inbounds i8, ptr %1, i64 16
  %18 = load ptr, ptr %arrayidx36, align 8
  store ptr %18, ptr %bp, align 8
  %arrayidx41 = getelementptr inbounds i8, ptr %1, i64 48
  %19 = load ptr, ptr %arrayidx41, align 8
  %20 = ptrtoint ptr %19 to i64
  %arrayidx43 = getelementptr inbounds i8, ptr %f, i64 48
  store i64 %20, ptr %arrayidx43, align 8
  %arrayidx46 = getelementptr inbounds i8, ptr %1, i64 24
  %21 = load ptr, ptr %arrayidx46, align 8
  %arrayidx48 = getelementptr inbounds i8, ptr %f, i64 64
  store ptr %21, ptr %arrayidx48, align 8
  br label %cleanup

if.end49:                                         ; preds = %if.end21
  %arrayidx56 = getelementptr inbounds i8, ptr %1, i64 16
  %22 = load ptr, ptr %arrayidx56, align 8
  %arrayidx59 = getelementptr inbounds i8, ptr %1, i64 24
  %23 = load ptr, ptr %arrayidx59, align 8
  %call.i = tail call ptr @unison_jit_settle(ptr noundef %22) #8
  %call1.i = tail call ptr @unison_jit_settle(ptr noundef %23) #8
  %24 = ptrtoint ptr %call.i to i64
  %and.i.i = and i64 %24, 7
  %cmp.i = icmp eq i64 %and.i.i, 0
  br i1 %cmp.i, label %if.then.i, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %if.end49
  %25 = ptrtoint ptr %call1.i to i64
  %and.i26.i = and i64 %25, 7
  %cmp4.i = icmp eq i64 %and.i26.i, 0
  br i1 %cmp4.i, label %if.then.i, label %if.end.i

if.then.i:                                        ; preds = %lor.lhs.false.i, %if.end49
  %cond.i = phi ptr [ %call1.i, %lor.lhs.false.i ], [ %call.i, %if.end49 ]
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.3, ptr noundef %cond.i) #8
  store i32 0, ptr %s, align 8
  br label %cleanup

if.end.i:                                         ; preds = %lor.lhs.false.i
  %and.i28.i = and i64 %24, -8
  %26 = inttoptr i64 %and.i28.i to ptr
  %payload.i = getelementptr inbounds i8, ptr %26, i64 8
  %27 = load ptr, ptr %payload.i, align 8
  %and.i29.i = and i64 %25, -8
  %28 = inttoptr i64 %and.i29.i to ptr
  %payload9.i = getelementptr inbounds i8, ptr %28, i64 8
  %29 = load ptr, ptr %payload9.i, align 8
  %ptrs.i = getelementptr inbounds i8, ptr %29, i64 8
  %30 = load i64, ptr %ptrs.i, align 8
  %n.i = getelementptr inbounds i8, ptr %f, i64 8
  store i64 %30, ptr %n.i, align 8
  %payload11.i = getelementptr inbounds i8, ptr %27, i64 16
  store ptr %payload11.i, ptr %us, align 8
  %payload13.i = getelementptr inbounds i8, ptr %29, i64 24
  store ptr %payload13.i, ptr %bs, align 8
  store i32 1, ptr %rev, align 8
  br label %cleanup

cleanup:                                          ; preds = %if.end.i, %if.then.i, %if.then23, %if.then5, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 1, %if.then5 ], [ 1, %if.then23 ], [ 0, %if.then.i ], [ 1, %if.end.i ]
  ret i32 %retval.0
}

define internal fastcc i64 @uc_fieldwise(ptr nocapture noundef %s, ptr nocapture noundef readonly %f1, ptr nocapture noundef readonly %f2, i32 noundef %tyeq) #0 {
entry:
  %n = getelementptr inbounds i8, ptr %f1, i64 8
  %0 = load i64, ptr %n, align 8
  %n1 = getelementptr inbounds i8, ptr %f2, i64 8
  %1 = load i64, ptr %n1, align 8
  %cmp.not = icmp eq i64 %0, %1
  br i1 %cmp.not, label %for.cond.preheader, label %if.then

for.cond.preheader:                               ; preds = %entry
  %cmp557.not = icmp eq i64 %0, 0
  br i1 %cmp557.not, label %return, label %land.rhs.lr.ph

land.rhs.lr.ph:                                   ; preds = %for.cond.preheader
  %us.i = getelementptr inbounds i8, ptr %f1, i64 16
  %rev.i = getelementptr inbounds i8, ptr %f1, i64 32
  %bs.i = getelementptr inbounds i8, ptr %f1, i64 24
  %us.i39 = getelementptr inbounds i8, ptr %f2, i64 16
  %rev.i40 = getelementptr inbounds i8, ptr %f2, i64 32
  %bs.i48 = getelementptr inbounds i8, ptr %f2, i64 24
  br label %land.rhs

if.then:                                          ; preds = %entry
  %cmp.i = icmp ult i64 %0, %1
  %cmp1.i = icmp ugt i64 %0, %1
  %2 = zext i1 %cmp1.i to i64
  %cond2.i = select i1 %cmp.i, i64 -1, i64 %2
  br label %return

for.cond:                                         ; preds = %fld_b.exit56
  %inc = add nuw i64 %i.058, 1
  %3 = load i64, ptr %n, align 8
  %cmp5 = icmp ult i64 %inc, %3
  br i1 %cmp5, label %land.rhs, label %return

land.rhs:                                         ; preds = %land.rhs.lr.ph, %for.cond
  %4 = phi i64 [ %0, %land.rhs.lr.ph ], [ %3, %for.cond ]
  %retval.059 = phi i64 [ undef, %land.rhs.lr.ph ], [ %retval.0.call10, %for.cond ]
  %i.058 = phi i64 [ 0, %land.rhs.lr.ph ], [ %inc, %for.cond ]
  %5 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %5, 0
  br i1 %tobool.not, label %return, label %for.body

for.body:                                         ; preds = %land.rhs
  %6 = load ptr, ptr %us.i, align 8
  %7 = load i32, ptr %rev.i, align 8
  %tobool.not.i = icmp eq i32 %7, 0
  %8 = xor i64 %i.058, -1
  %sub1.i = add i64 %4, %8
  %cond.i = select i1 %tobool.not.i, i64 %i.058, i64 %sub1.i
  %arrayidx.i = getelementptr inbounds i64, ptr %6, i64 %cond.i
  %9 = load i64, ptr %arrayidx.i, align 8
  %10 = load ptr, ptr %bs.i, align 8
  %arrayidx.i38 = getelementptr inbounds ptr, ptr %10, i64 %cond.i
  %11 = load ptr, ptr %arrayidx.i38, align 8
  %12 = load ptr, ptr %us.i39, align 8
  %13 = load i32, ptr %rev.i40, align 8
  %tobool.not.i41 = icmp eq i32 %13, 0
  br i1 %tobool.not.i41, label %fld_u.exit47, label %cond.true.i42

cond.true.i42:                                    ; preds = %for.body
  %14 = load i64, ptr %n1, align 8
  %sub1.i44 = add i64 %14, %8
  br label %fld_u.exit47

fld_u.exit47:                                     ; preds = %for.body, %cond.true.i42
  %cond.i45 = phi i64 [ %sub1.i44, %cond.true.i42 ], [ %i.058, %for.body ]
  %arrayidx.i46 = getelementptr inbounds i64, ptr %12, i64 %cond.i45
  %15 = load i64, ptr %arrayidx.i46, align 8
  %16 = load ptr, ptr %bs.i48, align 8
  br i1 %tobool.not.i41, label %fld_b.exit56, label %cond.true.i51

cond.true.i51:                                    ; preds = %fld_u.exit47
  %17 = load i64, ptr %n1, align 8
  %sub1.i53 = add i64 %17, %8
  br label %fld_b.exit56

fld_b.exit56:                                     ; preds = %fld_u.exit47, %cond.true.i51
  %cond.i54 = phi i64 [ %sub1.i53, %cond.true.i51 ], [ %i.058, %fld_u.exit47 ]
  %arrayidx.i55 = getelementptr inbounds ptr, ptr %16, i64 %cond.i54
  %18 = load ptr, ptr %arrayidx.i55, align 8
  %call10 = tail call fastcc i64 @uc_val(ptr noundef nonnull %s, i64 noundef %9, ptr noundef %11, i64 noundef %15, ptr noundef %18, i32 noundef %tyeq)
  %cmp11.not = icmp eq i64 %call10, 0
  %retval.0.call10 = select i1 %cmp11.not, i64 %retval.059, i64 %call10
  br i1 %cmp11.not, label %for.cond, label %return

return:                                           ; preds = %land.rhs, %for.cond, %fld_b.exit56, %for.cond.preheader, %if.then
  %retval.3 = phi i64 [ %cond2.i, %if.then ], [ 0, %for.cond.preheader ], [ %retval.0.call10, %fld_b.exit56 ], [ 0, %for.cond ], [ 0, %land.rhs ]
  ret i64 %retval.3
}

define internal fastcc range(i32 0, 2) i32 @uc_segs(ptr nocapture noundef writeonly %s, ptr noundef %ubox0, ptr noundef %bbox0, ptr nocapture noundef writeonly %f) #0 {
entry:
  %call = tail call ptr @unison_jit_settle(ptr noundef %ubox0) #8
  %call1 = tail call ptr @unison_jit_settle(ptr noundef %bbox0) #8
  %0 = ptrtoint ptr %call to i64
  %and.i = and i64 %0, 7
  %cmp = icmp eq i64 %and.i, 0
  br i1 %cmp, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %1 = ptrtoint ptr %call1 to i64
  %and.i26 = and i64 %1, 7
  %cmp4 = icmp eq i64 %and.i26, 0
  br i1 %cmp4, label %if.then, label %if.end

if.then:                                          ; preds = %lor.lhs.false, %entry
  %cond = phi ptr [ %call1, %lor.lhs.false ], [ %call, %entry ]
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.3, ptr noundef %cond) #8
  store i32 0, ptr %s, align 8
  br label %cleanup

if.end:                                           ; preds = %lor.lhs.false
  %and.i28 = and i64 %0, -8
  %2 = inttoptr i64 %and.i28 to ptr
  %payload = getelementptr inbounds i8, ptr %2, i64 8
  %3 = load ptr, ptr %payload, align 8
  %and.i29 = and i64 %1, -8
  %4 = inttoptr i64 %and.i29 to ptr
  %payload9 = getelementptr inbounds i8, ptr %4, i64 8
  %5 = load ptr, ptr %payload9, align 8
  %ptrs = getelementptr inbounds i8, ptr %5, i64 8
  %6 = load i64, ptr %ptrs, align 8
  %n = getelementptr inbounds i8, ptr %f, i64 8
  store i64 %6, ptr %n, align 8
  %payload11 = getelementptr inbounds i8, ptr %3, i64 16
  %us12 = getelementptr inbounds i8, ptr %f, i64 16
  store ptr %payload11, ptr %us12, align 8
  %payload13 = getelementptr inbounds i8, ptr %5, i64 24
  %bs15 = getelementptr inbounds i8, ptr %f, i64 24
  store ptr %payload13, ptr %bs15, align 8
  %rev = getelementptr inbounds i8, ptr %f, i64 32
  store i32 1, ptr %rev, align 8
  br label %cleanup

cleanup:                                          ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 0, %if.then ], [ 1, %if.end ]
  ret i32 %retval.0
}

declare i64 @unison_jit_foreign_cmp(ptr noundef, ptr noundef, i64 noundef)

define internal fastcc i64 @uc_lists(ptr nocapture noundef %s, ptr noundef %d1, ptr noundef %d2, i32 noundef %tyeq) #0 {
entry:
  %0 = ptrtoint ptr %d1 to i64
  %and.i = and i64 %0, 7
  %cmp = icmp eq i64 %and.i, 2
  br i1 %cmp, label %cond.true, label %cond.end

cond.true:                                        ; preds = %entry
  %and.i44 = and i64 %0, -8
  %1 = inttoptr i64 %and.i44 to ptr
  %arrayidx = getelementptr inbounds i8, ptr %1, i64 32
  %2 = load ptr, ptr %arrayidx, align 8
  %3 = ptrtoint ptr %2 to i64
  %shr = ashr i64 %3, 8
  br label %cond.end

cond.end:                                         ; preds = %entry, %cond.true
  %cond = phi i64 [ %shr, %cond.true ], [ 0, %entry ]
  %4 = ptrtoint ptr %d2 to i64
  %and.i45 = and i64 %4, 7
  %cmp3 = icmp eq i64 %and.i45, 2
  br i1 %cmp3, label %cond.true4, label %cond.end10

cond.true4:                                       ; preds = %cond.end
  %and.i46 = and i64 %4, -8
  %5 = inttoptr i64 %and.i46 to ptr
  %arrayidx7 = getelementptr inbounds i8, ptr %5, i64 32
  %6 = load ptr, ptr %arrayidx7, align 8
  %7 = ptrtoint ptr %6 to i64
  %shr8 = ashr i64 %7, 8
  br label %cond.end10

cond.end10:                                       ; preds = %cond.end, %cond.true4
  %cond11 = phi i64 [ %shr8, %cond.true4 ], [ 0, %cond.end ]
  %cond16 = tail call i64 @llvm.smin.i64(i64 %cond, i64 %cond11)
  %cmp1761 = icmp sgt i64 %cond16, 0
  br i1 %cmp1761, label %land.rhs, label %cleanup22

for.cond:                                         ; preds = %uc_elems.exit
  %inc = add nuw nsw i64 %i.066, 1
  %exitcond.not = icmp eq i64 %inc, %cond16
  br i1 %exitcond.not, label %cleanup22, label %land.rhs

land.rhs:                                         ; preds = %cond.end10, %for.cond
  %retval.067 = phi i64 [ %retval.0.call20, %for.cond ], [ undef, %cond.end10 ]
  %i.066 = phi i64 [ %inc, %for.cond ], [ 0, %cond.end10 ]
  %b2.i.065 = phi ptr [ %b2.i.2, %for.cond ], [ undef, %cond.end10 ]
  %b1.i.064 = phi ptr [ %b1.i.1, %for.cond ], [ undef, %cond.end10 ]
  %u2.i.063 = phi i64 [ %u2.i.2, %for.cond ], [ undef, %cond.end10 ]
  %u1.i.062 = phi i64 [ %u1.i.1, %for.cond ], [ undef, %cond.end10 ]
  %8 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %8, 0
  br i1 %tobool.not, label %cleanup22, label %for.body

for.body:                                         ; preds = %land.rhs
  %call18 = tail call ptr @unison_jit_list_at(ptr noundef %d1, i64 noundef %i.066) #8
  %call19 = tail call ptr @unison_jit_list_at(ptr noundef %d2, i64 noundef %i.066) #8
  %9 = ptrtoint ptr %call18 to i64
  %and.i.i50 = and i64 %9, 7
  %cmp.not.i51 = icmp eq i64 %and.i.i50, 1
  br i1 %cmp.not.i51, label %lor.lhs.false.i54, label %if.then.i52

lor.lhs.false.i54:                                ; preds = %for.body
  %and.i11.i55 = and i64 %9, -8
  %10 = inttoptr i64 %and.i11.i55 to ptr
  %11 = load ptr, ptr %10, align 8
  %12 = ptrtoint ptr %11 to i64
  %13 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i56 = icmp eq i64 %13, %12
  br i1 %cmp2.not.i56, label %if.end.i57, label %if.then.i52

if.then.i52:                                      ; preds = %lor.lhs.false.i54, %for.body
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %call18) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit60

if.end.i57:                                       ; preds = %lor.lhs.false.i54
  %arrayidx.i58 = getelementptr inbounds i8, ptr %10, i64 16
  %14 = load ptr, ptr %arrayidx.i58, align 8
  %15 = ptrtoint ptr %14 to i64
  %payload5.i59 = getelementptr inbounds i8, ptr %10, i64 8
  %16 = load ptr, ptr %payload5.i59, align 8
  br label %uc_elem.exit60

uc_elem.exit60:                                   ; preds = %if.then.i52, %if.end.i57
  %u1.i.1 = phi i64 [ %15, %if.end.i57 ], [ %u1.i.062, %if.then.i52 ]
  %b1.i.1 = phi ptr [ %16, %if.end.i57 ], [ %b1.i.064, %if.then.i52 ]
  %tobool.not.i = phi i1 [ false, %if.end.i57 ], [ true, %if.then.i52 ]
  br i1 %tobool.not.i, label %uc_elems.exit, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %uc_elem.exit60
  %17 = ptrtoint ptr %call19 to i64
  %and.i.i = and i64 %17, 7
  %cmp.not.i = icmp eq i64 %and.i.i, 1
  br i1 %cmp.not.i, label %lor.lhs.false.i48, label %if.then.i

lor.lhs.false.i48:                                ; preds = %lor.lhs.false.i
  %and.i11.i = and i64 %17, -8
  %18 = inttoptr i64 %and.i11.i to ptr
  %19 = load ptr, ptr %18, align 8
  %20 = ptrtoint ptr %19 to i64
  %21 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i = icmp eq i64 %21, %20
  br i1 %cmp2.not.i, label %if.end.i49, label %if.then.i

if.then.i:                                        ; preds = %lor.lhs.false.i48, %lor.lhs.false.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %call19) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit

if.end.i49:                                       ; preds = %lor.lhs.false.i48
  %arrayidx.i = getelementptr inbounds i8, ptr %18, i64 16
  %22 = load ptr, ptr %arrayidx.i, align 8
  %23 = ptrtoint ptr %22 to i64
  %payload5.i = getelementptr inbounds i8, ptr %18, i64 8
  %24 = load ptr, ptr %payload5.i, align 8
  br label %uc_elem.exit

uc_elem.exit:                                     ; preds = %if.then.i, %if.end.i49
  %u2.i.1 = phi i64 [ %23, %if.end.i49 ], [ %u2.i.063, %if.then.i ]
  %b2.i.1 = phi ptr [ %24, %if.end.i49 ], [ %b2.i.065, %if.then.i ]
  %tobool2.not.i = phi i1 [ false, %if.end.i49 ], [ true, %if.then.i ]
  br i1 %tobool2.not.i, label %uc_elems.exit, label %if.end.i

if.end.i:                                         ; preds = %uc_elem.exit
  %call3.i = tail call fastcc i64 @uc_val(ptr noundef nonnull %s, i64 noundef %u1.i.1, ptr noundef %b1.i.1, i64 noundef %u2.i.1, ptr noundef %b2.i.1, i32 noundef %tyeq)
  br label %uc_elems.exit

uc_elems.exit:                                    ; preds = %uc_elem.exit60, %uc_elem.exit, %if.end.i
  %u2.i.2 = phi i64 [ %u2.i.063, %uc_elem.exit60 ], [ %u2.i.1, %uc_elem.exit ], [ %u2.i.1, %if.end.i ]
  %b2.i.2 = phi ptr [ %b2.i.065, %uc_elem.exit60 ], [ %b2.i.1, %uc_elem.exit ], [ %b2.i.1, %if.end.i ]
  %retval.0.i = phi i64 [ 0, %uc_elem.exit60 ], [ 0, %uc_elem.exit ], [ %call3.i, %if.end.i ]
  %cmp21.not = icmp eq i64 %retval.0.i, 0
  %retval.0.call20 = select i1 %cmp21.not, i64 %retval.067, i64 %retval.0.i
  br i1 %cmp21.not, label %for.cond, label %cleanup22

cleanup22:                                        ; preds = %uc_elems.exit, %for.cond, %land.rhs, %cond.end10
  %cond28 = phi i1 [ true, %cond.end10 ], [ false, %uc_elems.exit ], [ true, %for.cond ], [ true, %land.rhs ]
  %retval.2 = phi i64 [ undef, %cond.end10 ], [ %retval.0.call20, %uc_elems.exit ], [ %retval.0.call20, %for.cond ], [ %retval.067, %land.rhs ]
  %cmp.i = icmp slt i64 %cond, %cond11
  %cmp1.i = icmp sgt i64 %cond, %cond11
  %25 = zext i1 %cmp1.i to i64
  %cond2.i = select i1 %cmp.i, i64 -1, i64 %25
  %retval.3 = select i1 %cond28, i64 %cond2.i, i64 %retval.2
  ret i64 %retval.3
}

define internal fastcc i64 @uc_arrays(ptr nocapture noundef %s, ptr nocapture noundef readonly %a1, ptr nocapture noundef readonly %a2, i32 noundef %tyeq) #0 {
entry:
  %ptrs = getelementptr inbounds i8, ptr %a1, i64 8
  %0 = load i64, ptr %ptrs, align 8
  %ptrs1 = getelementptr inbounds i8, ptr %a2, i64 8
  %1 = load i64, ptr %ptrs1, align 8
  %. = tail call i64 @llvm.umin.i64(i64 %0, i64 %1)
  %cmp443.not = icmp eq i64 %., 0
  br i1 %cmp443.not, label %cleanup8, label %land.rhs.lr.ph

land.rhs.lr.ph:                                   ; preds = %entry
  %payload = getelementptr inbounds i8, ptr %a1, i64 24
  %payload5 = getelementptr inbounds i8, ptr %a2, i64 24
  br label %land.rhs

for.cond:                                         ; preds = %uc_elems.exit
  %inc = add nuw i64 %i.048, 1
  %exitcond.not = icmp eq i64 %inc, %.
  br i1 %exitcond.not, label %cleanup8, label %land.rhs

land.rhs:                                         ; preds = %land.rhs.lr.ph, %for.cond
  %retval.049 = phi i64 [ undef, %land.rhs.lr.ph ], [ %retval.0.call, %for.cond ]
  %i.048 = phi i64 [ 0, %land.rhs.lr.ph ], [ %inc, %for.cond ]
  %b2.i.047 = phi ptr [ undef, %land.rhs.lr.ph ], [ %b2.i.2, %for.cond ]
  %b1.i.046 = phi ptr [ undef, %land.rhs.lr.ph ], [ %b1.i.1, %for.cond ]
  %u2.i.045 = phi i64 [ undef, %land.rhs.lr.ph ], [ %u2.i.2, %for.cond ]
  %u1.i.044 = phi i64 [ undef, %land.rhs.lr.ph ], [ %u1.i.1, %for.cond ]
  %2 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %2, 0
  br i1 %tobool.not, label %cleanup8, label %for.body

for.body:                                         ; preds = %land.rhs
  %arrayidx = getelementptr inbounds [0 x ptr], ptr %payload, i64 0, i64 %i.048
  %3 = load ptr, ptr %arrayidx, align 8
  %arrayidx6 = getelementptr inbounds [0 x ptr], ptr %payload5, i64 0, i64 %i.048
  %4 = load ptr, ptr %arrayidx6, align 8
  %5 = ptrtoint ptr %3 to i64
  %and.i.i32 = and i64 %5, 7
  %cmp.not.i33 = icmp eq i64 %and.i.i32, 1
  br i1 %cmp.not.i33, label %lor.lhs.false.i36, label %if.then.i34

lor.lhs.false.i36:                                ; preds = %for.body
  %and.i11.i37 = and i64 %5, -8
  %6 = inttoptr i64 %and.i11.i37 to ptr
  %7 = load ptr, ptr %6, align 8
  %8 = ptrtoint ptr %7 to i64
  %9 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i38 = icmp eq i64 %9, %8
  br i1 %cmp2.not.i38, label %if.end.i39, label %if.then.i34

if.then.i34:                                      ; preds = %lor.lhs.false.i36, %for.body
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %3) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit42

if.end.i39:                                       ; preds = %lor.lhs.false.i36
  %arrayidx.i40 = getelementptr inbounds i8, ptr %6, i64 16
  %10 = load ptr, ptr %arrayidx.i40, align 8
  %11 = ptrtoint ptr %10 to i64
  %payload5.i41 = getelementptr inbounds i8, ptr %6, i64 8
  %12 = load ptr, ptr %payload5.i41, align 8
  br label %uc_elem.exit42

uc_elem.exit42:                                   ; preds = %if.then.i34, %if.end.i39
  %u1.i.1 = phi i64 [ %11, %if.end.i39 ], [ %u1.i.044, %if.then.i34 ]
  %b1.i.1 = phi ptr [ %12, %if.end.i39 ], [ %b1.i.046, %if.then.i34 ]
  %tobool.not.i = phi i1 [ false, %if.end.i39 ], [ true, %if.then.i34 ]
  br i1 %tobool.not.i, label %uc_elems.exit, label %lor.lhs.false.i

lor.lhs.false.i:                                  ; preds = %uc_elem.exit42
  %13 = ptrtoint ptr %4 to i64
  %and.i.i = and i64 %13, 7
  %cmp.not.i = icmp eq i64 %and.i.i, 1
  br i1 %cmp.not.i, label %lor.lhs.false.i30, label %if.then.i

lor.lhs.false.i30:                                ; preds = %lor.lhs.false.i
  %and.i11.i = and i64 %13, -8
  %14 = inttoptr i64 %and.i11.i to ptr
  %15 = load ptr, ptr %14, align 8
  %16 = ptrtoint ptr %15 to i64
  %17 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i = icmp eq i64 %17, %16
  br i1 %cmp2.not.i, label %if.end.i31, label %if.then.i

if.then.i:                                        ; preds = %lor.lhs.false.i30, %lor.lhs.false.i
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %4) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit

if.end.i31:                                       ; preds = %lor.lhs.false.i30
  %arrayidx.i = getelementptr inbounds i8, ptr %14, i64 16
  %18 = load ptr, ptr %arrayidx.i, align 8
  %19 = ptrtoint ptr %18 to i64
  %payload5.i = getelementptr inbounds i8, ptr %14, i64 8
  %20 = load ptr, ptr %payload5.i, align 8
  br label %uc_elem.exit

uc_elem.exit:                                     ; preds = %if.then.i, %if.end.i31
  %u2.i.1 = phi i64 [ %19, %if.end.i31 ], [ %u2.i.045, %if.then.i ]
  %b2.i.1 = phi ptr [ %20, %if.end.i31 ], [ %b2.i.047, %if.then.i ]
  %tobool2.not.i = phi i1 [ false, %if.end.i31 ], [ true, %if.then.i ]
  br i1 %tobool2.not.i, label %uc_elems.exit, label %if.end.i

if.end.i:                                         ; preds = %uc_elem.exit
  %call3.i = tail call fastcc i64 @uc_val(ptr noundef nonnull %s, i64 noundef %u1.i.1, ptr noundef %b1.i.1, i64 noundef %u2.i.1, ptr noundef %b2.i.1, i32 noundef %tyeq)
  br label %uc_elems.exit

uc_elems.exit:                                    ; preds = %uc_elem.exit42, %uc_elem.exit, %if.end.i
  %u2.i.2 = phi i64 [ %u2.i.045, %uc_elem.exit42 ], [ %u2.i.1, %uc_elem.exit ], [ %u2.i.1, %if.end.i ]
  %b2.i.2 = phi ptr [ %b2.i.047, %uc_elem.exit42 ], [ %b2.i.1, %uc_elem.exit ], [ %b2.i.1, %if.end.i ]
  %retval.0.i = phi i64 [ 0, %uc_elem.exit42 ], [ 0, %uc_elem.exit ], [ %call3.i, %if.end.i ]
  %cmp7.not = icmp eq i64 %retval.0.i, 0
  %retval.0.call = select i1 %cmp7.not, i64 %retval.049, i64 %retval.0.i
  br i1 %cmp7.not, label %for.cond, label %cleanup8

cleanup8:                                         ; preds = %uc_elems.exit, %for.cond, %land.rhs, %entry
  %cond14 = phi i1 [ true, %entry ], [ false, %uc_elems.exit ], [ true, %for.cond ], [ true, %land.rhs ]
  %retval.2 = phi i64 [ undef, %entry ], [ %retval.0.call, %uc_elems.exit ], [ %retval.0.call, %for.cond ], [ %retval.049, %land.rhs ]
  br i1 %cond14, label %for.end, label %cleanup13

for.end:                                          ; preds = %cleanup8
  %21 = load i64, ptr %ptrs, align 8
  %22 = load i64, ptr %ptrs1, align 8
  %cmp.i = icmp ult i64 %21, %22
  %cmp1.i = icmp ugt i64 %21, %22
  %23 = zext i1 %cmp1.i to i64
  %cond2.i = select i1 %cmp.i, i64 -1, i64 %23
  br label %cleanup13

cleanup13:                                        ; preds = %cleanup8, %for.end
  %retval.3 = phi i64 [ %cond2.i, %for.end ], [ %retval.2, %cleanup8 ]
  ret i64 %retval.3
}

define internal fastcc i64 @uc_barrays(ptr nocapture noundef readonly %a1, ptr nocapture noundef readonly %a2) #6 {
entry:
  %payload = getelementptr inbounds i8, ptr %a1, i64 16
  %payload1 = getelementptr inbounds i8, ptr %a2, i64 16
  %bytes = getelementptr inbounds i8, ptr %a1, i64 8
  %0 = load i64, ptr %bytes, align 8
  %bytes3 = getelementptr inbounds i8, ptr %a2, i64 8
  %1 = load i64, ptr %bytes3, align 8
  %. = tail call i64 @llvm.umin.i64(i64 %0, i64 %1)
  %cmp6.not42 = icmp ne i64 %., 0
  br i1 %cmp6.not42, label %for.body.preheader, label %cleanup

for.body.preheader:                               ; preds = %entry
  %2 = load i8, ptr %payload, align 1
  %3 = load i8, ptr %payload1, align 1
  %cmp9.not53 = icmp eq i8 %2, %3
  br i1 %cmp9.not53, label %for.cond, label %if.then

for.cond:                                         ; preds = %for.body.preheader, %for.body
  %i.04354 = phi i64 [ %inc, %for.body ], [ 0, %for.body.preheader ]
  %inc = add nuw i64 %i.04354, 1
  %exitcond.not = icmp eq i64 %inc, %.
  br i1 %exitcond.not, label %cleanup, label %for.body

for.body:                                         ; preds = %for.cond
  %arrayidx = getelementptr inbounds i8, ptr %payload, i64 %inc
  %4 = load i8, ptr %arrayidx, align 1
  %arrayidx7 = getelementptr inbounds i8, ptr %payload1, i64 %inc
  %5 = load i8, ptr %arrayidx7, align 1
  %cmp9.not = icmp eq i8 %4, %5
  br i1 %cmp9.not, label %for.cond, label %if.then.loopexit

if.then.loopexit:                                 ; preds = %for.body
  %cmp6.not.le = icmp ult i64 %inc, %.
  br label %if.then

if.then:                                          ; preds = %if.then.loopexit, %for.body.preheader
  %cmp6.not44.lcssa = phi i1 [ %cmp6.not42, %for.body.preheader ], [ %cmp6.not.le, %if.then.loopexit ]
  %.lcssa50 = phi i8 [ %2, %for.body.preheader ], [ %4, %if.then.loopexit ]
  %.lcssa = phi i8 [ %3, %for.body.preheader ], [ %5, %if.then.loopexit ]
  %cmp15 = icmp ult i8 %.lcssa50, %.lcssa
  %conv18 = select i1 %cmp15, i64 -1, i64 1
  br label %cleanup

cleanup:                                          ; preds = %for.cond, %entry, %if.then
  %cmp6.not40 = phi i1 [ %cmp6.not44.lcssa, %if.then ], [ %cmp6.not42, %entry ], [ false, %for.cond ]
  %retval.0 = phi i64 [ %conv18, %if.then ], [ undef, %entry ], [ undef, %for.cond ]
  %cmp = icmp ult i64 %0, %1
  %cmp1.i = icmp ugt i64 %0, %1
  %6 = zext i1 %cmp1.i to i64
  %cond2.i = select i1 %cmp, i64 -1, i64 %6
  %retval.1 = select i1 %cmp6.not40, i64 %retval.0, i64 %cond2.i
  ret i64 %retval.1
}

declare ptr @unison_jit_list_at(ptr noundef, i64 noundef)

define internal fastcc i32 @ue_fieldwise(ptr nocapture noundef %s, ptr nocapture noundef readonly %f1, ptr nocapture noundef readonly %f2) #0 {
entry:
  %n = getelementptr inbounds i8, ptr %f1, i64 8
  %0 = load i64, ptr %n, align 8
  %n1 = getelementptr inbounds i8, ptr %f2, i64 8
  %1 = load i64, ptr %n1, align 8
  %cmp.not = icmp eq i64 %0, %1
  br i1 %cmp.not, label %for.cond.preheader, label %return

for.cond.preheader:                               ; preds = %entry
  %cmp349.not = icmp eq i64 %0, 0
  br i1 %cmp349.not, label %for.end.critedge, label %land.rhs.lr.ph

land.rhs.lr.ph:                                   ; preds = %for.cond.preheader
  %us.i = getelementptr inbounds i8, ptr %f1, i64 16
  %rev.i = getelementptr inbounds i8, ptr %f1, i64 32
  %bs.i = getelementptr inbounds i8, ptr %f1, i64 24
  %us.i31 = getelementptr inbounds i8, ptr %f2, i64 16
  %rev.i32 = getelementptr inbounds i8, ptr %f2, i64 32
  %bs.i40 = getelementptr inbounds i8, ptr %f2, i64 24
  br label %land.rhs

for.cond:                                         ; preds = %fld_b.exit48
  %inc = add nuw i64 %i.050, 1
  %2 = load i64, ptr %n, align 8
  %cmp3 = icmp ult i64 %inc, %2
  br i1 %cmp3, label %land.rhs, label %for.end.critedge

land.rhs:                                         ; preds = %land.rhs.lr.ph, %for.cond
  %3 = phi i64 [ %0, %land.rhs.lr.ph ], [ %2, %for.cond ]
  %i.050 = phi i64 [ 0, %land.rhs.lr.ph ], [ %inc, %for.cond ]
  %4 = load i32, ptr %s, align 8
  %tobool.not = icmp eq i32 %4, 0
  br i1 %tobool.not, label %for.end.critedge, label %for.body

for.body:                                         ; preds = %land.rhs
  %5 = load ptr, ptr %us.i, align 8
  %6 = load i32, ptr %rev.i, align 8
  %tobool.not.i = icmp eq i32 %6, 0
  %7 = xor i64 %i.050, -1
  %sub1.i = add i64 %3, %7
  %cond.i = select i1 %tobool.not.i, i64 %i.050, i64 %sub1.i
  %arrayidx.i = getelementptr inbounds i64, ptr %5, i64 %cond.i
  %8 = load i64, ptr %arrayidx.i, align 8
  %9 = load ptr, ptr %bs.i, align 8
  %arrayidx.i30 = getelementptr inbounds ptr, ptr %9, i64 %cond.i
  %10 = load ptr, ptr %arrayidx.i30, align 8
  %11 = load ptr, ptr %us.i31, align 8
  %12 = load i32, ptr %rev.i32, align 8
  %tobool.not.i33 = icmp eq i32 %12, 0
  br i1 %tobool.not.i33, label %fld_u.exit39, label %cond.true.i34

cond.true.i34:                                    ; preds = %for.body
  %13 = load i64, ptr %n1, align 8
  %sub1.i36 = add i64 %13, %7
  br label %fld_u.exit39

fld_u.exit39:                                     ; preds = %for.body, %cond.true.i34
  %cond.i37 = phi i64 [ %sub1.i36, %cond.true.i34 ], [ %i.050, %for.body ]
  %arrayidx.i38 = getelementptr inbounds i64, ptr %11, i64 %cond.i37
  %14 = load i64, ptr %arrayidx.i38, align 8
  %15 = load ptr, ptr %bs.i40, align 8
  br i1 %tobool.not.i33, label %fld_b.exit48, label %cond.true.i43

cond.true.i43:                                    ; preds = %fld_u.exit39
  %16 = load i64, ptr %n1, align 8
  %sub1.i45 = add i64 %16, %7
  br label %fld_b.exit48

fld_b.exit48:                                     ; preds = %fld_u.exit39, %cond.true.i43
  %cond.i46 = phi i64 [ %sub1.i45, %cond.true.i43 ], [ %i.050, %fld_u.exit39 ]
  %arrayidx.i47 = getelementptr inbounds ptr, ptr %15, i64 %cond.i46
  %17 = load ptr, ptr %arrayidx.i47, align 8
  %call7 = tail call fastcc i32 @ue_val(ptr noundef nonnull %s, i64 noundef %8, ptr noundef %10, i64 noundef %14, ptr noundef %17)
  %tobool8.not = icmp eq i32 %call7, 0
  br i1 %tobool8.not, label %return, label %for.cond

for.end.critedge:                                 ; preds = %for.cond, %land.rhs, %for.cond.preheader
  %18 = load i32, ptr %s, align 8
  br label %return

return:                                           ; preds = %fld_b.exit48, %entry, %for.end.critedge
  %retval.1 = phi i32 [ %18, %for.end.critedge ], [ 0, %entry ], [ 0, %fld_b.exit48 ]
  ret i32 %retval.1
}

declare i64 @unison_jit_foreign_eq(ptr noundef, ptr noundef, i64 noundef)

define internal fastcc i32 @ue_elems(ptr nocapture noundef %s, ptr noundef %x1, ptr noundef %x2) #0 {
entry:
  %0 = ptrtoint ptr %x1 to i64
  %and.i.i = and i64 %0, 7
  %cmp.not.i = icmp eq i64 %and.i.i, 1
  br i1 %cmp.not.i, label %lor.lhs.false.i, label %if.then.i

lor.lhs.false.i:                                  ; preds = %entry
  %and.i11.i = and i64 %0, -8
  %1 = inttoptr i64 %and.i11.i to ptr
  %2 = load ptr, ptr %1, align 8
  %3 = ptrtoint ptr %2 to i64
  %4 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i = icmp eq i64 %4, %3
  br i1 %cmp2.not.i, label %if.end.i, label %if.then.i

if.then.i:                                        ; preds = %lor.lhs.false.i, %entry
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %x1) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit

if.end.i:                                         ; preds = %lor.lhs.false.i
  %arrayidx.i = getelementptr inbounds i8, ptr %1, i64 16
  %5 = load ptr, ptr %arrayidx.i, align 8
  %6 = ptrtoint ptr %5 to i64
  %payload5.i = getelementptr inbounds i8, ptr %1, i64 8
  %7 = load ptr, ptr %payload5.i, align 8
  br label %uc_elem.exit

uc_elem.exit:                                     ; preds = %if.then.i, %if.end.i
  %u1.0 = phi i64 [ %6, %if.end.i ], [ undef, %if.then.i ]
  %b1.0 = phi ptr [ %7, %if.end.i ], [ undef, %if.then.i ]
  %tobool.not = phi i1 [ false, %if.end.i ], [ true, %if.then.i ]
  br i1 %tobool.not, label %cleanup, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %uc_elem.exit
  %8 = ptrtoint ptr %x2 to i64
  %and.i.i9 = and i64 %8, 7
  %cmp.not.i10 = icmp eq i64 %and.i.i9, 1
  br i1 %cmp.not.i10, label %lor.lhs.false.i13, label %if.then.i11

lor.lhs.false.i13:                                ; preds = %lor.lhs.false
  %and.i11.i14 = and i64 %8, -8
  %9 = inttoptr i64 %and.i11.i14 to ptr
  %10 = load ptr, ptr %9, align 8
  %11 = ptrtoint ptr %10 to i64
  %12 = load i64, ptr getelementptr inbounds (i8, ptr @unison_jit_LF, i64 64), align 8
  %cmp2.not.i15 = icmp eq i64 %12, %11
  br i1 %cmp2.not.i15, label %if.end.i16, label %if.then.i11

if.then.i11:                                      ; preds = %lor.lhs.false.i13, %lor.lhs.false
  tail call void @unison_jit_cmp_fail(ptr noundef nonnull @.str.6, ptr noundef %x2) #8
  store i32 0, ptr %s, align 8
  br label %uc_elem.exit19

if.end.i16:                                       ; preds = %lor.lhs.false.i13
  %arrayidx.i17 = getelementptr inbounds i8, ptr %9, i64 16
  %13 = load ptr, ptr %arrayidx.i17, align 8
  %14 = ptrtoint ptr %13 to i64
  %payload5.i18 = getelementptr inbounds i8, ptr %9, i64 8
  %15 = load ptr, ptr %payload5.i18, align 8
  br label %uc_elem.exit19

uc_elem.exit19:                                   ; preds = %if.then.i11, %if.end.i16
  %u2.0 = phi i64 [ %14, %if.end.i16 ], [ undef, %if.then.i11 ]
  %b2.0 = phi ptr [ %15, %if.end.i16 ], [ undef, %if.then.i11 ]
  %tobool2.not = phi i1 [ false, %if.end.i16 ], [ true, %if.then.i11 ]
  br i1 %tobool2.not, label %cleanup, label %if.end

if.end:                                           ; preds = %uc_elem.exit19
  %call3 = tail call fastcc i32 @ue_val(ptr noundef %s, i64 noundef %u1.0, ptr noundef %b1.0, i64 noundef %u2.0, ptr noundef %b2.0)
  br label %cleanup

cleanup:                                          ; preds = %uc_elem.exit, %uc_elem.exit19, %if.end
  %retval.0 = phi i32 [ %call3, %if.end ], [ 0, %uc_elem.exit19 ], [ 0, %uc_elem.exit ]
  ret i32 %retval.0
}

declare i64 @llvm.smin.i64(i64, i64) #7

declare i64 @llvm.umin.i64(i64, i64) #7

attributes #0 = { nounwind }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nofree nosync nounwind memory(readwrite, inaccessiblemem: none) }
attributes #4 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #5 = { inlinehint mustprogress nofree norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none) }
attributes #6 = { nofree norecurse nosync nounwind memory(argmem: read) }
attributes #7 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #8 = { nounwind }
